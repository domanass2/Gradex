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
(define-constant err-credential-not-found (err u113))
(define-constant err-credential-exists (err u114))
(define-constant err-template-not-found (err u115))
(define-constant err-criteria-not-met (err u116))
(define-constant err-credential-expired (err u117))
(define-constant err-invalid-template (err u118))

(define-data-var admin principal contract-owner)
(define-data-var next-exam-id uint u1)
(define-data-var next-appeal-id uint u1)
(define-data-var appeal-deadline-blocks uint u1000)
(define-data-var next-credential-id uint u1)
(define-data-var next-template-id uint u1)

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

;; Credential template system for defining achievement types
(define-map CredentialTemplates
    uint
    {
        name: (string-ascii 100),
        description: (string-ascii 250),
        credential-type: (string-ascii 30), ;; "certificate", "badge", "diploma"
        subject-requirement: (optional (string-ascii 30)),
        min-score-requirement: uint,
        expiry-blocks: (optional uint), ;; blocks until credential expires
        issuer-signature: (string-ascii 100),
        created-at: uint,
        active: bool
    }
)

;; Individual credentials issued to students
(define-map IssuedCredentials
    uint
    {
        template-id: uint,
        recipient: principal,
        exam-id: uint,
        achieved-score: uint,
        issued-at: uint,
        expires-at: (optional uint),
        verification-hash: (string-ascii 64),
        status: (string-ascii 20) ;; "active", "revoked", "expired"
    }
)

;; Track which credentials each student has earned
(define-map StudentCredentials
    {student: principal, template-id: uint}
    {
        credential-id: uint,
        earned-at: uint
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

;; Read-only functions for credential system
(define-read-only (get-credential-template (template-id uint))
    (map-get? CredentialTemplates template-id)
)

(define-read-only (get-issued-credential (credential-id uint))
    (map-get? IssuedCredentials credential-id)
)

(define-read-only (get-student-credential (student principal) (template-id uint))
    (map-get? StudentCredentials {student: student, template-id: template-id})
)

(define-read-only (is-credential-expired (credential-id uint))
    (match (get-issued-credential credential-id)
        credential
            (match (get expires-at credential)
                expiry (>= stacks-block-height expiry)
                false
            )
        true
    )
)

(define-read-only (can-earn-credential (student principal) (template-id uint) (exam-id uint))
    (match (get-credential-template template-id)
        template
            (match (get-exam-result exam-id student)
                result
                    (match (get-exam exam-id)
                        exam
                            (let ((existing-cred (get-student-credential student template-id)))
                                (and 
                                    (get active template)
                                    (get verified result)
                                    (>= (get score result) (get min-score-requirement template))
                                    (match (get subject-requirement template)
                                        req-subject (is-eq (get subject exam) req-subject)
                                        true
                                    )
                                    (is-none existing-cred)
                                )
                            )
                        false
                    )
                false
            )
        false
    )
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

;; Create a new credential template for automatic issuance
(define-public (create-credential-template 
    (name (string-ascii 100))
    (description (string-ascii 250))
    (credential-type (string-ascii 30))
    (subject-requirement (optional (string-ascii 30)))
    (min-score-requirement uint)
    (expiry-blocks (optional uint))
    (issuer-signature (string-ascii 100))
)
    (let ((template-id (var-get next-template-id)))
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (asserts! (> min-score-requirement u0) err-invalid-score)
        (asserts! (or (is-eq credential-type "certificate") 
                     (is-eq credential-type "badge") 
                     (is-eq credential-type "diploma")) err-invalid-template)
        (map-set CredentialTemplates
            template-id
            {
                name: name,
                description: description,
                credential-type: credential-type,
                subject-requirement: subject-requirement,
                min-score-requirement: min-score-requirement,
                expiry-blocks: expiry-blocks,
                issuer-signature: issuer-signature,
                created-at: stacks-block-height,
                active: true
            }
        )
        (var-set next-template-id (+ template-id u1))
        (ok template-id)
    )
)

;; Automatically issue credential when student meets criteria
(define-public (auto-issue-credential (student principal) (template-id uint) (exam-id uint))
    (let (
        (credential-id (var-get next-credential-id))
        (template (unwrap! (get-credential-template template-id) err-template-not-found))
        (result (unwrap! (get-exam-result exam-id student) err-no-result-found))
        (exam (unwrap! (get-exam exam-id) err-not-found))
        (verification-hash (concat "GRAD" (int-to-ascii credential-id)))
        (expires-at (match (get expiry-blocks template)
            blocks (some (+ stacks-block-height blocks))
            none
        ))
    )
        (asserts! (can-earn-credential student template-id exam-id) err-criteria-not-met)
        (asserts! (is-none (get-student-credential student template-id)) err-credential-exists)
        (map-set IssuedCredentials
            credential-id
            {
                template-id: template-id,
                recipient: student,
                exam-id: exam-id,
                achieved-score: (get score result),
                issued-at: stacks-block-height,
                expires-at: expires-at,
                verification-hash: verification-hash,
                status: "active"
            }
        )
        (map-set StudentCredentials
            {student: student, template-id: template-id}
            {
                credential-id: credential-id,
                earned-at: stacks-block-height
            }
        )
        (var-set next-credential-id (+ credential-id u1))
        (ok credential-id)
    )
)

;; Manually issue credential by admin/verifier
(define-public (manual-issue-credential 
    (student principal) 
    (template-id uint) 
    (exam-id uint)
    (custom-score uint)
)
    (let (
        (credential-id (var-get next-credential-id))
        (template (unwrap! (get-credential-template template-id) err-template-not-found))
        (exam (unwrap! (get-exam exam-id) err-not-found))
        (verification-hash (concat "GRAD" (int-to-ascii credential-id)))
        (expires-at (match (get expiry-blocks template)
            blocks (some (+ stacks-block-height blocks))
            none
        ))
    )
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (get active template) err-invalid-template)
        (asserts! (>= custom-score (get min-score-requirement template)) err-criteria-not-met)
        (asserts! (is-none (get-student-credential student template-id)) err-credential-exists)
        (map-set IssuedCredentials
            credential-id
            {
                template-id: template-id,
                recipient: student,
                exam-id: exam-id,
                achieved-score: custom-score,
                issued-at: stacks-block-height,
                expires-at: expires-at,
                verification-hash: verification-hash,
                status: "active"
            }
        )
        (map-set StudentCredentials
            {student: student, template-id: template-id}
            {
                credential-id: credential-id,
                earned-at: stacks-block-height
            }
        )
        (var-set next-credential-id (+ credential-id u1))
        (ok credential-id)
    )
)

;; Revoke a credential
(define-public (revoke-credential (credential-id uint))
    (let (
        (credential (unwrap! (get-issued-credential credential-id) err-credential-not-found))
    )
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (is-eq (get status credential) "active") err-credential-expired)
        (ok (map-set IssuedCredentials
            credential-id
            (merge credential {status: "revoked"})
        ))
    )
)

;; Deactivate a credential template
(define-public (deactivate-template (template-id uint))
    (let (
        (template (unwrap! (get-credential-template template-id) err-template-not-found))
    )
        (asserts! (is-eq tx-sender (var-get admin)) err-owner-only)
        (ok (map-set CredentialTemplates
            template-id
            (merge template {active: false})
        ))
    )
)

;; Verify credential authenticity
(define-public (verify-credential (credential-id uint) (expected-hash (string-ascii 64)))
    (let (
        (credential (unwrap! (get-issued-credential credential-id) err-credential-not-found))
        (stored-hash (get verification-hash credential))
    )
        (asserts! (is-eq stored-hash expected-hash) err-unauthorized)
        (asserts! (is-eq (get status credential) "active") err-credential-expired)
        (asserts! (not (is-credential-expired credential-id)) err-credential-expired)
        (ok true)
    )
)

