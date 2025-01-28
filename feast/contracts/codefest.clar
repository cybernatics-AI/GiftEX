;; Holiday Gift Exchange Protocol

;; Core Constants
(define-constant admin-wallet tx-sender)
(define-constant error-unauthorized (err u201))
(define-constant error-duplicate-entry (err u202))
(define-constant error-insufficient-funds (err u203))
(define-constant error-unknown-member (err u204))
(define-constant error-matching-complete (err u205))
(define-constant error-locked-period (err u206))
(define-constant error-gift-redeemed (err u207))
(define-constant error-group-size (err u208))
(define-constant error-matching-error (err u209))

;; Protocol State
(define-data-var enrollment-active bool true)
(define-data-var distribution-timestamp uint u1703462400) ;; Dec 24, 2024 00:00:00 UTC
(define-data-var group-size-min uint u3)
(define-data-var entry-fee-min uint u100)
(define-data-var member-total uint u0)
(define-data-var matching-progress uint u0)

;; Data Storage
(define-map members principal 
  {
    active: bool,
    deposit: uint,
    matched: bool,
    collected: bool,
    sequence: uint
  }
)

(define-map member-sequence uint principal)
(define-map gift-assignments principal principal) ;; Gifter -> Recipient
(define-map gift-sources principal principal) ;; Recipient -> Gifter

;; Helper Functions
(define-private (check-membership (wallet principal))
  (default-to false (get active (map-get? members wallet)))
)

(define-private (get-deposit-amount (wallet principal))
  (default-to u0 (get deposit (map-get? members wallet)))
)

(define-private (check-matching-status (wallet principal))
  (default-to false (get matched (map-get? members wallet)))
)

;; Core Protocol Functions
(define-public (join-exchange (deposit uint))
  (let (
    (participant tx-sender)
    (current-members (var-get member-total))
  )
    (asserts! (var-get enrollment-active) error-matching-complete)
    (asserts! (>= deposit (var-get entry-fee-min)) error-insufficient-funds)
    (asserts! (not (check-membership participant)) error-duplicate-entry)
    
    (try! (stx-transfer? deposit participant (as-contract tx-sender)))
    
    (map-set members participant {
      active: true,
      deposit: deposit,
      matched: false,
      collected: false,
      sequence: current-members
    })
    
    (map-set member-sequence current-members participant)
    (var-set member-total (+ current-members u1))
    
    (ok true))
)

(define-public (process-match)
  (let (
    (caller tx-sender)
    (total-members (var-get member-total))
    (current-match (var-get matching-progress))
  )
    (asserts! (is-admin) error-unauthorized)
    (asserts! (>= total-members (var-get group-size-min)) error-group-size)
    (asserts! (< current-match total-members) error-matching-error)
    
    (let (
      (current-member (unwrap! (map-get? member-sequence current-match) error-matching-error))
      (next-match (mod (+ current-match u1) total-members))
      (next-member (unwrap! (map-get? member-sequence next-match) error-matching-error))
    )
      (map-set gift-assignments current-member next-member)
      (map-set gift-sources next-member current-member)
      
      (map-set members current-member 
        (merge (unwrap! (map-get? members current-member) error-unknown-member)
          { matched: true }))
      
      (var-set matching-progress (+ current-match u1))
      
      (if (is-eq (+ current-match u1) total-members)
        (var-set enrollment-active false)
        true)
      
      (ok true)))
)

(define-public (check-recipient)
  (let ((participant tx-sender))
    (asserts! (>= block-height (var-get distribution-timestamp)) error-locked-period)
    (asserts! (check-membership participant) error-unknown-member)
    (asserts! (check-matching-status participant) error-matching-complete)
    
    (ok (unwrap! (map-get? gift-sources participant) error-unknown-member)))
)

(define-public (collect-gift)
  (let (
    (recipient tx-sender)
    (member-info (unwrap! (map-get? members recipient) error-unknown-member))
  )
    (asserts! (>= block-height (var-get distribution-timestamp)) error-locked-period)
    (asserts! (not (get collected member-info)) error-gift-redeemed)
    
    (let ((gifter (unwrap! (map-get? gift-sources recipient) error-unknown-member)))
      (try! (as-contract (stx-transfer? 
        (get deposit (unwrap! (map-get? members gifter) error-unknown-member))
        tx-sender
        recipient)))
      
      (map-set members recipient 
        (merge member-info { collected: true }))
      
      (ok true)))
)

(define-public (exit-exchange)
  (let (
    (participant tx-sender)
    (member-info (unwrap! (map-get? members participant) error-unknown-member))
  )
    (asserts! (var-get enrollment-active) error-matching-complete)
    (asserts! (not (get matched member-info)) error-matching-complete)
    
    (try! (as-contract (stx-transfer? 
      (get deposit member-info)
      tx-sender
      participant)))
    
    (map-delete members participant)
    (var-set member-total (- (var-get member-total) u1))
    (ok true))
)

;; Query Functions
(define-read-only (get-member-details (participant principal))
  (map-get? members participant)
)

(define-read-only (is-admin)
  (is-eq tx-sender admin-wallet)
)

(define-read-only (get-total-members)
  (var-get member-total)
)

(define-read-only (get-matching-progress)
  (var-get matching-progress)
)