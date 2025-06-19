  ;; title: UBI (Universal Basic Income)
  ;; version: 1.0
  ;; summary: A decentralized UBI distribution system
  ;; description: Provides periodic token distributions to verified users


  ;; The amount of tokens to distribute per cycle
  (define-constant UBI_AMOUNT u1000)

  ;; Distribution cycle in blocks (approximately 1 week with 10 min blocks)
  (define-constant CYCLE_LENGTH u1008)

  ;; Maximum number of registered users
  (define-constant MAX_USERS u10000)

  ;; Error codes
  (define-constant ERR_NOT_AUTHORIZED (err u100))
  (define-constant ERR_ALREADY_REGISTERED (err u101))
  (define-constant ERR_NOT_REGISTERED (err u102))
  (define-constant ERR_ALREADY_CLAIMED (err u103))
  (define-constant ERR_MAX_USERS_REACHED (err u104))
  (define-constant ERR_INVALID_CREDENTIALS (err u105))
  (define-constant ERR_CYCLE_NOT_COMPLETE (err u106))
  (define-constant ERR_INSUFFICIENT_FUNDS (err u107))
  (define-constant ERR_INVALID_PROPOSAL (err u108))
  (define-constant ERR_ALREADY_VOTED (err u109))
  (define-constant ERR_LOCKED (err u110))
  (define-constant ERR_NO_WITHDRAWAL (err u111))


  ;; Contract administrator
  (define-data-var contract-owner principal tx-sender)

  ;; Total number of registered users
  (define-data-var user-count uint u0)

  ;; Current cycle number
  (define-data-var current-cycle uint u0)

  ;; Contract treasury balance
  (define-data-var treasury-balance uint u0)

  ;; -----------------
  ;; Data Maps
  ;; -----------------

  ;; Map to track registered users
  (define-map registered-users principal bool)

  ;; Map to store user verification status
  (define-map user-verification 
    { user: principal } 
    { verified: bool, registration-block: uint })

  ;; Map to track claims for each cycle
  (define-map claims 
    { user: principal, cycle: uint } 
    { claimed: bool, amount: uint, stacks-block-height: uint })

  ;; Map to store user profiles
  (define-map user-profiles
    { user: principal }
    { name: (string-ascii 64), 
      email-hash: (buff 32), 
      last-verification: uint })

  ;; -----------------
  ;; Public Functions
  ;; -----------------

  ;; Initialize the contract with initial funds
  (define-public (initialize (initial-funds uint))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (var-set treasury-balance initial-funds)
      (ok true)))

  ;; Register a new user for UBI
  (define-public (register-user 
                  (name (string-ascii 64)) 
                  (email-hash (buff 32)))
    (let ((user-count-current (var-get user-count)))
      (asserts! (< user-count-current MAX_USERS) ERR_MAX_USERS_REACHED)
      (asserts! (not (default-to false (map-get? registered-users tx-sender))) ERR_ALREADY_REGISTERED)
      
      ;; Register the user
      (map-set registered-users tx-sender true)
      (map-set user-verification 
        { user: tx-sender } 
        { verified: false, registration-block: stacks-block-height })
      
      ;; Store user profile
      (map-set user-profiles
        { user: tx-sender }
        { name: name, 
          email-hash: email-hash, 
          last-verification: stacks-block-height })
      
      ;; Increment user count
      (var-set user-count (+ user-count-current u1))
      
      (ok true)))

  ;; Verify a user (admin only)
  (define-public (verify-user (user principal))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (asserts! (default-to false (map-get? registered-users user)) ERR_NOT_REGISTERED)
      
      (map-set user-verification 
        { user: user } 
        { verified: true, 
          registration-block: (get registration-block (default-to 
                                                      { verified: false, registration-block: u0 } 
                                                      (map-get? user-verification { user: user }))) })
      
      (ok true)))

  ;; Claim UBI for the current cycle
  (define-public (claim-ubi)
    (let ((current-cycle-num (var-get current-cycle))
          (treasury (var-get treasury-balance))
          (user-verified (get verified (default-to 
                                        { verified: false, registration-block: u0 } 
                                        (map-get? user-verification { user: tx-sender })))))
      
      ;; Check if user is registered and verified
      (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
      (asserts! user-verified ERR_INVALID_CREDENTIALS)
      
      ;; Check if user has already claimed for this cycle
      (asserts! (not (default-to false 
                                (get claimed (map-get? claims { user: tx-sender, cycle: current-cycle-num })))) 
                ERR_ALREADY_CLAIMED)
      
      ;; Check if treasury has enough funds
      (asserts! (>= treasury UBI_AMOUNT) ERR_INSUFFICIENT_FUNDS)
      
      ;; Record the claim
      (map-set claims 
        { user: tx-sender, cycle: current-cycle-num } 
        { claimed: true, amount: UBI_AMOUNT, stacks-block-height: stacks-block-height })
      
      ;; Update treasury balance
      (var-set treasury-balance (- treasury UBI_AMOUNT))
      
      (ok UBI_AMOUNT)))

  ;; Advance to the next cycle (admin only)
  (define-public (advance-cycle)
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
      (asserts! (>= stacks-block-height (+ (cycle-start-block (var-get current-cycle)) CYCLE_LENGTH)) ERR_CYCLE_NOT_COMPLETE)
      
      (var-set current-cycle (+ (var-get current-cycle) u1))
      (ok (var-get current-cycle))))

  ;; Add funds to the treasury
  (define-public (fund-treasury (amount uint))
    (begin
      (var-set treasury-balance (+ (var-get treasury-balance) amount))
      (ok true)))

  ;; Update user profile
  (define-public (update-profile 
                  (name (string-ascii 64)) 
                  (email-hash (buff 32)))
    (begin
      (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
      
      (map-set user-profiles
        { user: tx-sender }
        { name: name, 
          email-hash: email-hash, 
          last-verification: stacks-block-height })
      
      (ok true)))

  ;; -----------------
  ;; Read-only Functions
  ;; -----------------

  ;; Get user verification status
  (define-read-only (get-verification-status (user principal))
    (default-to 
      { verified: false, registration-block: u0 } 
      (map-get? user-verification { user: user })))

  ;; Check if user has claimed UBI for a specific cycle
  (define-read-only (has-claimed-for-cycle (user principal) (cycle uint))
    (default-to 
      { claimed: false, amount: u0, stacks-block-height: u0 } 
      (map-get? claims { user: user, cycle: cycle })))

  ;; Get user profile
  (define-read-only (get-user-profile (user principal))
    (default-to 
      { name: "", email-hash: 0x, last-verification: u0 } 
      (map-get? user-profiles { user: user })))

  ;; Get current cycle information
  (define-read-only (get-current-cycle)
    (var-get current-cycle))

  ;; Get treasury balance
  (define-read-only (get-treasury-balance)
    (var-get treasury-balance))

  ;; Get total registered users
  (define-read-only (get-user-count)
    (var-get user-count))

  ;; Calculate the start block for a given cycle
  (define-read-only (cycle-start-block (cycle uint))
    (* cycle CYCLE_LENGTH))

  ;; Check if user is registered
  (define-read-only (is-registered (user principal))
    (default-to false (map-get? registered-users user)))


  ;; Calculate the next claim block for a user
  (define-private (next-claim-block (user principal))
    (let ((last-claim (get stacks-block-height 
                          (default-to 
                            { claimed: false, amount: u0, stacks-block-height: u0 } 
                            (map-get? claims { user: user, cycle: (var-get current-cycle) })))))
      (+ last-claim CYCLE_LENGTH)))


;; Add to data vars section
(define-data-var contract-paused bool false)
(define-data-var proposal-count uint u0)

;; Add these public functions
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)))

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)))


;; Add to constants
(define-constant REFERRAL_BONUS u100)

;; Add to data maps
(define-map referrals
  { referrer: principal }
  { count: uint, total-bonus: uint })

;; Add this public function
(define-public (register-with-referral (name (string-ascii 64)) (email-hash (buff 32)) (referrer principal))
  (let ((referrer-data (default-to { count: u0, total-bonus: u0 } (map-get? referrals { referrer: referrer }))))
    (try! (register-user name email-hash))
    (map-set referrals 
      { referrer: referrer }
      { count: (+ (get count referrer-data) u1),
        total-bonus: (+ (get total-bonus referrer-data) REFERRAL_BONUS) })
    (ok true)))



;; Add to constants
(define-constant TIER1_THRESHOLD u10)
(define-constant TIER2_THRESHOLD u50)
(define-constant TIER1_BONUS u100)
(define-constant TIER2_BONUS u200)

;; Add this read-only function
(define-read-only (calculate-tier-bonus (claim-count uint))
  (if (>= claim-count TIER2_THRESHOLD)
      TIER2_BONUS
      (if (>= claim-count TIER1_THRESHOLD)
          TIER1_BONUS
          u0)))


;; Add to data maps
(define-map proposals
  { id: uint }
  { title: (string-ascii 64),
    votes: uint,
    active: bool })

(define-map user-votes
  { user: principal, proposal-id: uint }
  { voted: bool })

;; Add these public functions
(define-public (create-proposal (title (string-ascii 64)))
  (let ((proposal-id (var-get proposal-count)))
    (asserts! (is-registered tx-sender) ERR_NOT_REGISTERED)
    (map-set proposals
      { id: proposal-id }
      { title: title, votes: u0, active: true })
    (var-set proposal-count (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint))
  (let ((proposal (default-to { title: "", votes: u0, active: false }
                             (map-get? proposals { id: proposal-id }))))
    (asserts! (get active proposal) ERR_INVALID_PROPOSAL)
    (asserts! (not (default-to false (get voted (map-get? user-votes { user: tx-sender, proposal-id: proposal-id })))) ERR_ALREADY_VOTED)
    (map-set proposals
      { id: proposal-id }
      { title: (get title proposal),
        votes: (+ (get votes proposal) u1),
        active: true })
    (ok true)))


;; Add to data maps
(define-map withdrawal-requests
  { user: principal }
  { amount: uint,
    unlock-height: uint })

;; Add these public functions
(define-public (request-withdrawal (amount uint))
  (let ((lock-period u144)) ;; 24 hours in blocks
    (map-set withdrawal-requests
      { user: tx-sender }
      { amount: amount,
        unlock-height: (+ stacks-block-height lock-period) })
    (ok true)))

(define-public (execute-withdrawal)
  (let ((request (default-to { amount: u0, unlock-height: u0 }
                            (map-get? withdrawal-requests { user: tx-sender }))))
    (asserts! (>= stacks-block-height (get unlock-height request)) ERR_LOCKED)
    (asserts! (> (get amount request) u0) ERR_NO_WITHDRAWAL)
    (ok (get amount request))))


;; Add to data maps
(define-map user-achievements
  { user: principal }
  { consecutive-claims: uint,
    total-claimed: uint,
    special-status: bool })

;; Add this public function
(define-public (update-achievements)
  (let ((user-data (default-to 
                     { consecutive-claims: u0, total-claimed: u0, special-status: false }
                     (map-get? user-achievements { user: tx-sender }))))
    (map-set user-achievements
      { user: tx-sender }
      { consecutive-claims: (+ (get consecutive-claims user-data) u1),
        total-claimed: (+ (get total-claimed user-data) UBI_AMOUNT),
        special-status: (>= (+ (get consecutive-claims user-data) u1) u12) })
    (ok true)))


(define-map delegates 
  { user: principal }
  { delegate: principal, active: bool })

(define-public (set-delegate (delegate-address principal))
  (begin
    (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
    (map-set delegates
      { user: tx-sender }
      { delegate: delegate-address, active: true })
    (ok true)))

(define-public (remove-delegate)
  (begin
    (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
    (map-set delegates
      { user: tx-sender }
      { delegate: tx-sender, active: false })
    (ok true)))

(define-public (claim-ubi-delegated (user principal))
  (let ((delegate-info (default-to 
                        { delegate: tx-sender, active: false }
                        (map-get? delegates { user: user }))))
    (asserts! (is-eq (get delegate delegate-info) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get active delegate-info) ERR_NOT_AUTHORIZED)
    (try! (claim-ubi))
    (ok true)))


(define-data-var pause-until uint u0)
(define-constant MAX_PAUSE_DURATION u1008)

(define-public (emergency-pause (duration uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= duration MAX_PAUSE_DURATION) ERR_INVALID_PROPOSAL)
    (var-set contract-paused true)
    (var-set pause-until (+ stacks-block-height duration))
    (ok true)))

(define-read-only (is-contract-paused)
  (if (>= stacks-block-height (var-get pause-until))
      false
      (var-get contract-paused)))

(define-public (force-unpause)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (>= stacks-block-height (var-get pause-until)) ERR_LOCKED)
    (var-set contract-paused false)
    (var-set pause-until u0)
    (ok true)))

(define-constant MIN_SIGNERS u2)
(define-constant MAX_SIGNERS u5)
(define-constant LARGE_WITHDRAWAL_THRESHOLD u10000)
(define-constant ERR_INSUFFICIENT_SIGNERS (err u112))
(define-constant ERR_INVALID_SIGNER (err u113))
(define-constant ERR_TRANSACTION_NOT_FOUND (err u114))
(define-constant ERR_ALREADY_SIGNED (err u115))

(define-data-var multisig-enabled bool false)
(define-data-var required-signatures uint u2)
(define-data-var transaction-nonce uint u0)

(define-map authorized-signers principal bool)

(define-map pending-transactions
  { tx-id: uint }
  { action: (string-ascii 32),
    amount: uint,
    target: principal,
    signatures: uint,
    executed: bool,
    created-at: uint })

(define-map transaction-signatures
  { tx-id: uint, signer: principal }
  { signed: bool, timestamp: uint })

;; Add to data maps
(define-map user-activity
  { user: principal }
  { last-activity: uint,
    activity-count: uint }) 


(define-public (enable-multisig (signers (list 5 principal)) (required uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= required MIN_SIGNERS) (<= required MAX_SIGNERS)) ERR_INSUFFICIENT_SIGNERS)
    (asserts! (>= (len signers) required) ERR_INSUFFICIENT_SIGNERS)
    
    (map add-signer signers)
    (var-set required-signatures required)
    (var-set multisig-enabled true)
    (ok true)))

(define-public (propose-treasury-withdrawal (amount uint) (recipient principal))
  (let ((tx-id (var-get transaction-nonce)))
    (asserts! (var-get multisig-enabled) ERR_NOT_AUTHORIZED)
    (asserts! (default-to false (map-get? authorized-signers tx-sender)) ERR_INVALID_SIGNER)
    (asserts! (>= amount LARGE_WITHDRAWAL_THRESHOLD) ERR_INVALID_PROPOSAL)
    
    (map-set pending-transactions
      { tx-id: tx-id }
      { action: "withdraw",
        amount: amount,
        target: recipient,
        signatures: u1,
        executed: false,
        created-at: stacks-block-height })
    
    (map-set transaction-signatures
      { tx-id: tx-id, signer: tx-sender }
      { signed: true, timestamp: stacks-block-height })
    
    (var-set transaction-nonce (+ tx-id u1))
    (ok tx-id)))

(define-public (sign-transaction (tx-id uint))
  (let ((tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR_TRANSACTION_NOT_FOUND)))
    (asserts! (default-to false (map-get? authorized-signers tx-sender)) ERR_INVALID_SIGNER)
    (asserts! (not (get executed tx-data)) ERR_INVALID_PROPOSAL)
    (asserts! (not (default-to false (get signed (map-get? transaction-signatures { tx-id: tx-id, signer: tx-sender })))) ERR_ALREADY_SIGNED)
    
    (map-set transaction-signatures
      { tx-id: tx-id, signer: tx-sender }
      { signed: true, timestamp: stacks-block-height })
    
    (map-set pending-transactions
      { tx-id: tx-id }
      { action: (get action tx-data),
        amount: (get amount tx-data),
        target: (get target tx-data),
        signatures: (+ (get signatures tx-data) u1),
        executed: false,
        created-at: (get created-at tx-data) })
    
    (ok true)))

(define-public (execute-transaction (tx-id uint))
  (let ((tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR_TRANSACTION_NOT_FOUND)))
    (asserts! (default-to false (map-get? authorized-signers tx-sender)) ERR_INVALID_SIGNER)
    (asserts! (not (get executed tx-data)) ERR_INVALID_PROPOSAL)
    (asserts! (>= (get signatures tx-data) (var-get required-signatures)) ERR_INSUFFICIENT_SIGNERS)
    (asserts! (>= (var-get treasury-balance) (get amount tx-data)) ERR_INSUFFICIENT_FUNDS)
    
    (var-set treasury-balance (- (var-get treasury-balance) (get amount tx-data)))
    
    (map-set pending-transactions
      { tx-id: tx-id }
      { action: (get action tx-data),
        amount: (get amount tx-data),
        target: (get target tx-data),
        signatures: (get signatures tx-data),
        executed: true,
        created-at: (get created-at tx-data) })
    
    (ok (get amount tx-data))))

(define-public (revoke-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (map-delete authorized-signers signer)
    (ok true)))

(define-private (add-signer (signer principal))
  (map-set authorized-signers signer true))

(define-read-only (is-authorized-signer (signer principal))
  (default-to false (map-get? authorized-signers signer)))

(define-read-only (get-transaction-details (tx-id uint))
  (map-get? pending-transactions { tx-id: tx-id }))

(define-read-only (has-signed-transaction (tx-id uint) (signer principal))
  (default-to false (get signed (map-get? transaction-signatures { tx-id: tx-id, signer: signer }))))

(define-read-only (get-multisig-status)
  { enabled: (var-get multisig-enabled),
    required-signatures: (var-get required-signatures),
    current-nonce: (var-get transaction-nonce) })