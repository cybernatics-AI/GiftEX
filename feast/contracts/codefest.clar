;; Holiday Gift Exchange Protocol v1
;; Basic implementation with core functionality

;; Constants
(define-constant admin-wallet tx-sender)
(define-constant error-unauthorized (err u201))
(define-constant error-duplicate-entry (err u202))
(define-constant error-insufficient-funds (err u203))
(define-constant error-unknown-member (err u204))

;; State Variables
(define-data-var member-total uint u0)
(define-data-var entry-fee-min uint u100)

;; Storage
(define-map members principal 
  {
    active: bool,
    deposit: uint
  }
)

;; Helper Functions
(define-private (check-membership (wallet principal))
  (default-to false (get active (map-get? members wallet)))
)

;; Core Functions
(define-public (join-exchange (deposit uint))
  (let (
    (participant tx-sender)
    (current-members (var-get member-total))
  )
    (asserts! (>= deposit (var-get entry-fee-min)) error-insufficient-funds)
    (asserts! (not (check-membership participant)) error-duplicate-entry)
    
    (try! (stx-transfer? deposit participant (as-contract tx-sender)))
    
    (map-set members participant {
      active: true,
      deposit: deposit
    })
    
    (var-set member-total (+ current-members u1))
    
    (ok true))
)

(define-public (exit-exchange)
  (let (
    (participant tx-sender)
    (member-info (unwrap! (map-get? members participant) error-unknown-member))
  )    
    (try! (as-contract (stx-transfer? 
      (get deposit member-info)
      tx-sender
      participant)))
    
    (map-delete members participant)
    (var-set member-total (- (var-get member-total) u1))
    (ok true))
)

;; Read-only Functions
(define-read-only (get-member-details (participant principal))
  (map-get? members participant)
)

(define-read-only (is-admin)
  (is-eq tx-sender admin-wallet)
)

(define-read-only (get-total-members)
  (var-get member-total)
)