(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-score (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-status (err u105))
(define-constant err-appeal-not-found (err u106))
(define-constant err-appeal-expired (err u107))
(define-constant err-appeal-closed (err u108))
(define-constant err-appeal-exists (err u109))
(define-constant err-no-result-found (err u110))
(define-constant err-invalid-decision (err u111))
(define-constant err-appeal-pending (err u112))

(define-data-var admin principal contract-owner)
(define-data-var next-exam-id uint u1)
(define-data-var next-appeal-id uint u1)
(define-data-var appeal-deadline-blocks uint u1000)

(define-map Exams
    uint 
    {
        exam-name: (string-ascii 50),
        subject: (string-ascii 30),
        max-score: uint,
        date: uint,
        status: (string-ascii 10)
    }
)

(define-map ExamResults
    {exam-id: uint, student: principal}
    {
        score: uint,
        verified: bool,
        timestamp: uint
    }
)

(define-map Verifiers
    principal 
    bool
)

(define-map StudentProfiles
    principal
    {
        name: (string-ascii 50),
        institution: (string-ascii 50),
        registered-at: uint
    }
)

(define-map Appeals
    uint
    {
        exam-id: uint,
        student: principal,
        original-score: uint,
        reason: (string-ascii 200),
        status: (string-ascii 20),
        submitted-at: uint,
        deadline: uint,
        reviewed-by: (optional principal),
        decision: (optional (string-ascii 20)),
        new-score: (optional uint),
        final-decision-at: (optional uint)
    }
)

(define-map AppealVotes
    {appeal-id: uint, verifier: principal}
    {
        vote: (string-ascii 10),
        justification: (string-ascii 150),
        voted-at: uint
    }
)

(define-read-only (get-exam (exam-id uint))
    (map-get? Exams exam-id)
)

(define-read-only (get-exam-result (exam-id uint) (student principal))
    (map-get? ExamResults {exam-id: exam-id, student: student})
)

(define-read-only (get-student-profile (student principal))
    (map-get? StudentProfiles student)
)

(define-read-only (is-verifier (account principal))
    (default-to false (map-get? Verifiers account))
)

(define-read-only (get-appeal (appeal-id uint))
    (map-get? Appeals appeal-id)
)

(define-read-only (get-appeal-vote (appeal-id uint) (verifier principal))
    (map-get? AppealVotes {appeal-id: appeal-id, verifier: verifier})
)

(define-read-only (is-appeal-expired (appeal-id uint))
    (match (get-appeal appeal-id)
        appeal (> stacks-block-height (get deadline appeal))
        true
    )
)

(define-read-only (get-appeal-deadline-blocks)
    (var-get appeal-deadline-blocks)
)

(define-public (register-student (name (string-ascii 50)) (institution (string-ascii 50)))
    (begin
        (asserts! (is-none (get-student-profile tx-sender)) err-already-exists)
        (ok (map-set StudentProfiles
            tx-sender
            {
                name: name,
                institution: institution,
                registered-at: stacks-block-height
            }
        ))
    )
)

(define-public (create-exam (exam-name (string-ascii 50)) (subject (string-ascii 30)) (max-score uint) (date uint))
    (let ((exam-id (var-get next-exam-id)))
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (asserts! (> max-score u0) err-invalid-score)
        (map-set Exams
            exam-id
            {
                exam-name: exam-name,
                subject: subject,
                max-score: max-score,
                date: date,
                status: "active"
            }
        )
        (var-set next-exam-id (+ exam-id u1))
        (ok exam-id)
    )
)

(define-public (submit-result (exam-id uint) (student principal) (score uint))
    (let ((exam (unwrap! (get-exam exam-id) err-not-found)))
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (<= score (get max-score exam)) err-invalid-score)
        (asserts! (is-eq (get status exam) "active") err-invalid-status)
        (ok (map-set ExamResults
            {exam-id: exam-id, student: student}
            {
                score: score,
                verified: false,
                timestamp: stacks-block-height
            }
        ))
    )
)

(define-public (verify-result (exam-id uint) (student principal))
    (let (
        (result (unwrap! (get-exam-result exam-id student) err-not-found))
        (exam (unwrap! (get-exam exam-id) err-not-found))
    )
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (is-eq (get status exam) "active") err-invalid-status)
        (ok (map-set ExamResults
            {exam-id: exam-id, student: student}
            (merge result {verified: true})
        ))
    )
)

(define-public (add-verifier (account principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (ok (map-set Verifiers account true))
    )
)

(define-public (remove-verifier (account principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (ok (map-set Verifiers account false))
    )
)

(define-public (close-exam (exam-id uint))
    (let ((exam (unwrap! (get-exam exam-id) err-not-found)))
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (ok (map-set Exams
            exam-id
            (merge exam {status: "closed"})
        ))
    )
)

(define-public (transfer-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (ok (var-set admin new-admin))
    )
)

(define-public (submit-appeal (exam-id uint) (reason (string-ascii 200)))
    (let (
        (appeal-id (var-get next-appeal-id))
        (result (unwrap! (get-exam-result exam-id tx-sender) err-no-result-found))
        (exam (unwrap! (get-exam exam-id) err-not-found))
        (current-block stacks-block-height)
        (deadline (+ current-block (var-get appeal-deadline-blocks)))
    )
        (asserts! (is-none (get-appeal appeal-id)) err-appeal-exists)
        (asserts! (is-some (get-student-profile tx-sender)) err-unauthorized)
        (asserts! (is-eq (get status exam) "active") err-invalid-status)
        (asserts! (get verified result) err-unauthorized)
        (map-set Appeals
            appeal-id
            {
                exam-id: exam-id,
                student: tx-sender,
                original-score: (get score result),
                reason: reason,
                status: "pending",
                submitted-at: current-block,
                deadline: deadline,
                reviewed-by: none,
                decision: none,
                new-score: none,
                final-decision-at: none
            }
        )
        (var-set next-appeal-id (+ appeal-id u1))
        (ok appeal-id)
    )
)

(define-public (vote-on-appeal (appeal-id uint) (vote (string-ascii 10)) (justification (string-ascii 150)))
    (let (
        (appeal (unwrap! (get-appeal appeal-id) err-appeal-not-found))
        (existing-vote (get-appeal-vote appeal-id tx-sender))
    )
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (is-eq (get status appeal) "pending") err-appeal-closed)
        (asserts! (not (is-appeal-expired appeal-id)) err-appeal-expired)
        (asserts! (is-none existing-vote) err-already-exists)
        (asserts! (or (is-eq vote "approve") (is-eq vote "reject")) err-invalid-decision)
        (ok (map-set AppealVotes
            {appeal-id: appeal-id, verifier: tx-sender}
            {
                vote: vote,
                justification: justification,
                voted-at: stacks-block-height
            }
        ))
    )
)

(define-public (finalize-appeal (appeal-id uint) (decision (string-ascii 20)) (new-score (optional uint)))
    (let (
        (appeal (unwrap! (get-appeal appeal-id) err-appeal-not-found))
        (exam (unwrap! (get-exam (get exam-id appeal)) err-not-found))
        (student (get student appeal))
        (exam-id (get exam-id appeal))
    )
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (is-eq (get status appeal) "pending") err-appeal-closed)
        (asserts! (or (is-eq decision "approved") (is-eq decision "rejected")) err-invalid-decision)
        (asserts! (if (is-eq decision "approved") (is-some new-score) true) err-invalid-score)
        (asserts! (if (is-some new-score) (<= (unwrap-panic new-score) (get max-score exam)) true) err-invalid-score)
        (map-set Appeals
            appeal-id
            (merge appeal {
                status: decision,
                reviewed-by: (some tx-sender),
                decision: (some decision),
                new-score: new-score,
                final-decision-at: (some stacks-block-height)
            })
        )
        (if (is-eq decision "approved")
            (begin
                (map-set ExamResults
                    {exam-id: exam-id, student: student}
                    {
                        score: (unwrap-panic new-score),
                        verified: true,
                        timestamp: stacks-block-height
                    }
                )
                (ok "appeal-approved")
            )
            (ok "appeal-rejected")
        )
    )
)

(define-public (withdraw-appeal (appeal-id uint))
    (let (
        (appeal (unwrap! (get-appeal appeal-id) err-appeal-not-found))
    )
        (asserts! (is-eq tx-sender (get student appeal)) err-unauthorized)
        (asserts! (is-eq (get status appeal) "pending") err-appeal-closed)
        (asserts! (not (is-appeal-expired appeal-id)) err-appeal-expired)
        (ok (map-set Appeals
            appeal-id
            (merge appeal {
                status: "withdrawn",
                final-decision-at: (some stacks-block-height)
            })
        ))
    )
)

(define-public (set-appeal-deadline (new-deadline uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (asserts! (> new-deadline u0) err-invalid-score)
        (ok (var-set appeal-deadline-blocks new-deadline))
    )
)

(define-public (extend-appeal-deadline (appeal-id uint) (additional-blocks uint))
    (let (
        (appeal (unwrap! (get-appeal appeal-id) err-appeal-not-found))
        (new-deadline (+ (get deadline appeal) additional-blocks))
    )
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (asserts! (is-eq (get status appeal) "pending") err-appeal-closed)
        (asserts! (> additional-blocks u0) err-invalid-score)
        (ok (map-set Appeals
            appeal-id
            (merge appeal {deadline: new-deadline})
        ))
    )
)