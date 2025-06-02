(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-score (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-status (err u105))

(define-data-var admin principal contract-owner)
(define-data-var next-exam-id uint u1)

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