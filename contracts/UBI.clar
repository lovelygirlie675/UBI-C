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

  
;; UBI Savings Account Constants
(define-constant DEFAULT_SAVINGS_RATE u20)
(define-constant MAX_SAVINGS_RATE u50)
(define-constant SAVINGS_INTEREST_RATE u5)
(define-constant MIN_SAVINGS_WITHDRAWAL u50)
(define-constant ERR_SAVINGS_INSUFFICIENT (err u116))
(define-constant ERR_INVALID_SAVINGS_RATE (err u117))
(define-constant ERR_SAVINGS_LOCKED (err u118))

;; Savings Data Maps
(define-map user-savings-accounts
  { user: principal }
  { balance: uint,
    savings-rate: uint,
    total-deposited: uint,
    last-interest-block: uint,
    lock-until: uint })

(define-map savings-transactions
  { user: principal, transaction-id: uint }
  { amount: uint,
    transaction-type: (string-ascii 10),
    block-height: uint })

;; Savings Data Variables
(define-data-var total-savings-pool uint u0)
(define-data-var next-transaction-id uint u0)


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



;; Set User Savings Rate
(define-public (set-savings-rate (rate uint))
  (begin
    (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
    (asserts! (<= rate MAX_SAVINGS_RATE) ERR_INVALID_SAVINGS_RATE)
    
    (let ((current-account (default-to 
                           { balance: u0, savings-rate: DEFAULT_SAVINGS_RATE, 
                             total-deposited: u0, last-interest-block: stacks-block-height, 
                             lock-until: u0 }
                           (map-get? user-savings-accounts { user: tx-sender }))))
      (map-set user-savings-accounts
        { user: tx-sender }
        { balance: (get balance current-account),
          savings-rate: rate,
          total-deposited: (get total-deposited current-account),
          last-interest-block: (get last-interest-block current-account),
          lock-until: (get lock-until current-account) }))
    
    (ok rate)))

;; Enhanced Claim UBI with Automatic Savings
(define-public (claim-ubi-with-savings)
  (let ((current-cycle-num (var-get current-cycle))
        (treasury (var-get treasury-balance))
        (user-verified (get verified (default-to 
                                     { verified: false, registration-block: u0 } 
                                     (map-get? user-verification { user: tx-sender }))))
        (savings-account (default-to 
                         { balance: u0, savings-rate: DEFAULT_SAVINGS_RATE, 
                           total-deposited: u0, last-interest-block: stacks-block-height, 
                           lock-until: u0 }
                         (map-get? user-savings-accounts { user: tx-sender }))))
    
    (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
    (asserts! user-verified ERR_INVALID_CREDENTIALS)
    (asserts! (not (default-to false 
                               (get claimed (map-get? claims { user: tx-sender, cycle: current-cycle-num })))) 
              ERR_ALREADY_CLAIMED)
    (asserts! (>= treasury UBI_AMOUNT) ERR_INSUFFICIENT_FUNDS)
    
    (let ((savings-amount (/ (* UBI_AMOUNT (get savings-rate savings-account)) u100))
          (payout-amount (- UBI_AMOUNT savings-amount))
          (tx-id (var-get next-transaction-id)))
      
      (map-set claims 
        { user: tx-sender, cycle: current-cycle-num } 
        { claimed: true, amount: UBI_AMOUNT, stacks-block-height: stacks-block-height })
      
      (map-set user-savings-accounts
        { user: tx-sender }
        { balance: (+ (get balance savings-account) savings-amount),
          savings-rate: (get savings-rate savings-account),
          total-deposited: (+ (get total-deposited savings-account) savings-amount),
          last-interest-block: stacks-block-height,
          lock-until: (get lock-until savings-account) })
      
      (map-set savings-transactions
        { user: tx-sender, transaction-id: tx-id }
        { amount: savings-amount,
          transaction-type: "deposit",
          block-height: stacks-block-height })
      
      (var-set treasury-balance (- treasury UBI_AMOUNT))
      (var-set total-savings-pool (+ (var-get total-savings-pool) savings-amount))
      (var-set next-transaction-id (+ tx-id u1))
      
      (ok { payout: payout-amount, saved: savings-amount }))))

;; Withdraw from Savings Account
(define-public (withdraw-savings (amount uint))
  (let ((savings-account (unwrap! (map-get? user-savings-accounts { user: tx-sender }) ERR_NOT_REGISTERED))
        (tx-id (var-get next-transaction-id)))
    
    (asserts! (>= (get balance savings-account) amount) ERR_SAVINGS_INSUFFICIENT)
    (asserts! (>= amount MIN_SAVINGS_WITHDRAWAL) ERR_INVALID_PROPOSAL)
    (asserts! (>= stacks-block-height (get lock-until savings-account)) ERR_SAVINGS_LOCKED)
    
    (map-set user-savings-accounts
      { user: tx-sender }
      { balance: (- (get balance savings-account) amount),
        savings-rate: (get savings-rate savings-account),
        total-deposited: (get total-deposited savings-account),
        last-interest-block: (get last-interest-block savings-account),
        lock-until: (get lock-until savings-account) })
    
    (map-set savings-transactions
      { user: tx-sender, transaction-id: tx-id }
      { amount: amount,
        transaction-type: "withdraw",
        block-height: stacks-block-height })
    
    (var-set total-savings-pool (- (var-get total-savings-pool) amount))
    (var-set next-transaction-id (+ tx-id u1))
    
    (ok amount)))

;; Calculate and Apply Interest to Savings
(define-public (apply-savings-interest)
  (let ((savings-account (unwrap! (map-get? user-savings-accounts { user: tx-sender }) ERR_NOT_REGISTERED))
        (blocks-since-last-interest (- stacks-block-height (get last-interest-block savings-account)))
        (interest-cycles (/ blocks-since-last-interest CYCLE_LENGTH)))
    
    (asserts! (> interest-cycles u0) ERR_CYCLE_NOT_COMPLETE)
    
    (let ((current-balance (get balance savings-account))
          (interest-amount (/ (* current-balance SAVINGS_INTEREST_RATE interest-cycles) u100))
          (tx-id (var-get next-transaction-id)))
      
      (map-set user-savings-accounts
        { user: tx-sender }
        { balance: (+ current-balance interest-amount),
          savings-rate: (get savings-rate savings-account),
          total-deposited: (get total-deposited savings-account),
          last-interest-block: stacks-block-height,
          lock-until: (get lock-until savings-account) })
      
      (map-set savings-transactions
        { user: tx-sender, transaction-id: tx-id }
        { amount: interest-amount,
          transaction-type: "interest",
          block-height: stacks-block-height })
      
      (var-set next-transaction-id (+ tx-id u1))
      
      (ok interest-amount))))

;; Lock Savings for Higher Interest
(define-public (lock-savings (lock-cycles uint))
  (let ((savings-account (unwrap! (map-get? user-savings-accounts { user: tx-sender }) ERR_NOT_REGISTERED))
        (lock-blocks (* lock-cycles CYCLE_LENGTH)))
    
    (asserts! (> (get balance savings-account) u0) ERR_SAVINGS_INSUFFICIENT)
    (asserts! (and (>= lock-cycles u4) (<= lock-cycles u52)) ERR_INVALID_PROPOSAL)
    
    (map-set user-savings-accounts
      { user: tx-sender }
      { balance: (get balance savings-account),
        savings-rate: (get savings-rate savings-account),
        total-deposited: (get total-deposited savings-account),
        last-interest-block: (get last-interest-block savings-account),
        lock-until: (+ stacks-block-height lock-blocks) })
    
    (ok lock-blocks)))

;; Read-Only Functions for Savings

(define-read-only (get-savings-account (user principal))
  (default-to 
    { balance: u0, savings-rate: DEFAULT_SAVINGS_RATE, 
      total-deposited: u0, last-interest-block: u0, 
      lock-until: u0 }
    (map-get? user-savings-accounts { user: user })))

(define-read-only (get-savings-transaction (user principal) (transaction-id uint))
  (map-get? savings-transactions { user: user, transaction-id: transaction-id }))

(define-read-only (calculate-pending-interest (user principal))
  (let ((savings-account (get-savings-account user))
        (blocks-since-last-interest (- stacks-block-height (get last-interest-block savings-account)))
        (interest-cycles (/ blocks-since-last-interest CYCLE_LENGTH)))
    
    (if (> interest-cycles u0)
        (/ (* (get balance savings-account) SAVINGS_INTEREST_RATE interest-cycles) u100)
        u0)))

(define-read-only (get-total-savings-pool)
  (var-get total-savings-pool))

(define-read-only (is-savings-locked (user principal))
  (let ((savings-account (get-savings-account user)))
    (>= (get lock-until savings-account) stacks-block-height)))

;; =================
;; UBI INSURANCE POOL
;; =================

;; Insurance Pool Constants
(define-constant INSURANCE_CONTRIBUTION_RATE u5) ;; 5% of UBI amount per cycle
(define-constant MIN_INSURANCE_CLAIM u500) ;; Minimum emergency claim amount
(define-constant MAX_INSURANCE_CLAIM u5000) ;; Maximum emergency claim amount
(define-constant INSURANCE_ELIGIBILITY_CYCLES u6) ;; Must contribute for 6 cycles before claiming
(define-constant CLAIM_VOTING_PERIOD u1008) ;; 1 week voting period for claims
(define-constant REQUIRED_CLAIM_VOTES u3) ;; Minimum votes needed to approve claim
(define-constant MAX_CLAIMS_PER_YEAR u2) ;; Maximum claims per user per year

;; Insurance Error Codes
(define-constant ERR_INSURANCE_INSUFFICIENT (err u119))
(define-constant ERR_CLAIM_LIMIT_EXCEEDED (err u120))
(define-constant ERR_INSURANCE_INELIGIBLE (err u121))
(define-constant ERR_CLAIM_VOTING_ACTIVE (err u122))
(define-constant ERR_CLAIM_EXPIRED (err u123))
(define-constant ERR_DUPLICATE_VOTE (err u124))

;; Insurance Data Variables
(define-data-var insurance-pool-balance uint u0)
(define-data-var total-insurance-claims uint u0)
(define-data-var claim-request-counter uint u0)

;; Insurance Data Maps
(define-map insurance-contributors
  { user: principal }
  { total-contributed: uint,
    cycles-contributed: uint,
    last-contribution-cycle: uint,
    eligible-since: uint })

(define-map insurance-claims
  { claim-id: uint }
  { claimant: principal,
    amount: uint,
    reason: (string-ascii 128),
    status: (string-ascii 16),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    voting-ends: uint })

(define-map claim-votes
  { claim-id: uint, voter: principal }
  { vote: bool, ;; true = approve, false = deny
    timestamp: uint })

(define-map annual-claim-counts
  { user: principal, year: uint }
  { claims-made: uint })

(define-map insurance-payouts
  { user: principal, claim-id: uint }
  { amount: uint,
    payout-date: uint,
    reason: (string-ascii 128) })

;; Contribute to Insurance Pool (automatic with UBI claim)
(define-public (contribute-to-insurance)
  (let ((current-cycle-num (var-get current-cycle))
        (contribution-amount (/ (* UBI_AMOUNT INSURANCE_CONTRIBUTION_RATE) u100))
        (contributor-data (default-to 
                          { total-contributed: u0, cycles-contributed: u0, 
                            last-contribution-cycle: u0, eligible-since: u0 }
                          (map-get? insurance-contributors { user: tx-sender }))))
    
    ;; Verify user is registered and hasn't contributed this cycle
    (asserts! (default-to false (map-get? registered-users tx-sender)) ERR_NOT_REGISTERED)
    (asserts! (not (is-eq (get last-contribution-cycle contributor-data) current-cycle-num)) ERR_ALREADY_CLAIMED)
    
    ;; Update contributor record
    (let ((new-cycles (+ (get cycles-contributed contributor-data) u1)))
      (map-set insurance-contributors
        { user: tx-sender }
        { total-contributed: (+ (get total-contributed contributor-data) contribution-amount),
          cycles-contributed: new-cycles,
          last-contribution-cycle: current-cycle-num,
          eligible-since: (if (>= new-cycles INSURANCE_ELIGIBILITY_CYCLES)
                             (if (is-eq (get eligible-since contributor-data) u0)
                                stacks-block-height
                                (get eligible-since contributor-data))
                             u0) }))
    
    ;; Update insurance pool balance
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) contribution-amount))
    
    (ok contribution-amount)))

;; Submit Insurance Claim Request
(define-public (submit-insurance-claim (amount uint) (reason (string-ascii 128)))
  (let ((claim-id (var-get claim-request-counter))
        (current-year (/ stacks-block-height (* CYCLE_LENGTH u52))) ;; Approximate year calculation
        (contributor-data (unwrap! (map-get? insurance-contributors { user: tx-sender }) ERR_INSURANCE_INELIGIBLE))
        (annual-claims (default-to { claims-made: u0 } 
                                  (map-get? annual-claim-counts { user: tx-sender, year: current-year }))))
    
    ;; Verify eligibility and limits
    (asserts! (>= (get cycles-contributed contributor-data) INSURANCE_ELIGIBILITY_CYCLES) ERR_INSURANCE_INELIGIBLE)
    (asserts! (and (>= amount MIN_INSURANCE_CLAIM) (<= amount MAX_INSURANCE_CLAIM)) ERR_INVALID_PROPOSAL)
    (asserts! (< (get claims-made annual-claims) MAX_CLAIMS_PER_YEAR) ERR_CLAIM_LIMIT_EXCEEDED)
    (asserts! (>= (var-get insurance-pool-balance) amount) ERR_INSURANCE_INSUFFICIENT)
    
    ;; Create claim request
    (map-set insurance-claims
      { claim-id: claim-id }
      { claimant: tx-sender,
        amount: amount,
        reason: reason,
        status: "pending",
        votes-for: u0,
        votes-against: u0,
        created-at: stacks-block-height,
        voting-ends: (+ stacks-block-height CLAIM_VOTING_PERIOD) })
    
    ;; Update claim counter
    (var-set claim-request-counter (+ claim-id u1))
    
    (ok claim-id)))

;; Vote on Insurance Claim
(define-public (vote-on-insurance-claim (claim-id uint) (approve bool))
  (let ((claim-data (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_TRANSACTION_NOT_FOUND))
        (contributor-data (unwrap! (map-get? insurance-contributors { user: tx-sender }) ERR_INSURANCE_INELIGIBLE)))
    
    ;; Verify voting eligibility
    (asserts! (>= (get cycles-contributed contributor-data) INSURANCE_ELIGIBILITY_CYCLES) ERR_INSURANCE_INELIGIBLE)
    (asserts! (is-eq (get status claim-data) "pending") ERR_CLAIM_VOTING_ACTIVE)
    (asserts! (< stacks-block-height (get voting-ends claim-data)) ERR_CLAIM_EXPIRED)
    (asserts! (not (is-some (map-get? claim-votes { claim-id: claim-id, voter: tx-sender }))) ERR_DUPLICATE_VOTE)
    (asserts! (not (is-eq (get claimant claim-data) tx-sender)) ERR_NOT_AUTHORIZED) ;; Can't vote on own claim
    
    ;; Record vote
    (map-set claim-votes
      { claim-id: claim-id, voter: tx-sender }
      { vote: approve, timestamp: stacks-block-height })
    
    ;; Update claim vote counts
    (map-set insurance-claims
      { claim-id: claim-id }
      { claimant: (get claimant claim-data),
        amount: (get amount claim-data),
        reason: (get reason claim-data),
        status: (get status claim-data),
        votes-for: (if approve (+ (get votes-for claim-data) u1) (get votes-for claim-data)),
        votes-against: (if approve (get votes-against claim-data) (+ (get votes-against claim-data) u1)),
        created-at: (get created-at claim-data),
        voting-ends: (get voting-ends claim-data) })
    
    (ok approve)))

;; Process Insurance Claim (after voting period)
(define-public (process-insurance-claim (claim-id uint))
  (let ((claim-data (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_TRANSACTION_NOT_FOUND))
        (current-year (/ stacks-block-height (* CYCLE_LENGTH u52))))
    
    ;; Verify claim can be processed
    (asserts! (is-eq (get status claim-data) "pending") ERR_CLAIM_VOTING_ACTIVE)
    (asserts! (>= stacks-block-height (get voting-ends claim-data)) ERR_LOCKED)
    
    ;; Check if claim is approved
    (if (and (>= (get votes-for claim-data) REQUIRED_CLAIM_VOTES)
             (> (get votes-for claim-data) (get votes-against claim-data)))
        ;; Claim approved - process payout
        (begin
          (asserts! (>= (var-get insurance-pool-balance) (get amount claim-data)) ERR_INSURANCE_INSUFFICIENT)
          
          ;; Update claim status
          (map-set insurance-claims
            { claim-id: claim-id }
            { claimant: (get claimant claim-data),
              amount: (get amount claim-data),
              reason: (get reason claim-data),
              status: "approved",
              votes-for: (get votes-for claim-data),
              votes-against: (get votes-against claim-data),
              created-at: (get created-at claim-data),
              voting-ends: (get voting-ends claim-data) })
          
          ;; Record payout
          (map-set insurance-payouts
            { user: (get claimant claim-data), claim-id: claim-id }
            { amount: (get amount claim-data),
              payout-date: stacks-block-height,
              reason: (get reason claim-data) })
          
          ;; Update annual claim count
          (let ((annual-claims (default-to { claims-made: u0 } 
                                          (map-get? annual-claim-counts { user: (get claimant claim-data), year: current-year }))))
            (map-set annual-claim-counts
              { user: (get claimant claim-data), year: current-year }
              { claims-made: (+ (get claims-made annual-claims) u1) }))
          
          ;; Update pool balance and counters
          (var-set insurance-pool-balance (- (var-get insurance-pool-balance) (get amount claim-data)))
          (var-set total-insurance-claims (+ (var-get total-insurance-claims) u1))
          
          (ok { status: "approved", amount: (get amount claim-data) }))
        
        ;; Claim denied
        (begin
          (map-set insurance-claims
            { claim-id: claim-id }
            { claimant: (get claimant claim-data),
              amount: (get amount claim-data),
              reason: (get reason claim-data),
              status: "denied",
              votes-for: (get votes-for claim-data),
              votes-against: (get votes-against claim-data),
              created-at: (get created-at claim-data),
              voting-ends: (get voting-ends claim-data) })
          
          (ok { status: "denied", amount: u0 })))))

;; Emergency Withdraw from Insurance Pool (admin only)
(define-public (emergency-withdraw-insurance (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (var-get insurance-pool-balance) amount) ERR_INSURANCE_INSUFFICIENT)
    
    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) amount))
    
    (ok amount)))

;; Read-Only Functions for Insurance Pool

(define-read-only (get-insurance-contributor (user principal))
  (default-to 
    { total-contributed: u0, cycles-contributed: u0, 
      last-contribution-cycle: u0, eligible-since: u0 }
    (map-get? insurance-contributors { user: user })))

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id }))

(define-read-only (get-claim-vote (claim-id uint) (voter principal))
  (map-get? claim-votes { claim-id: claim-id, voter: voter }))

(define-read-only (get-insurance-payout (user principal) (claim-id uint))
  (map-get? insurance-payouts { user: user, claim-id: claim-id }))

(define-read-only (get-annual-claims (user principal) (year uint))
  (default-to { claims-made: u0 } 
              (map-get? annual-claim-counts { user: user, year: year })))

(define-read-only (get-insurance-pool-status)
  { balance: (var-get insurance-pool-balance),
    total-claims: (var-get total-insurance-claims),
    pending-claims: (var-get claim-request-counter) })

(define-read-only (is-insurance-eligible (user principal))
  (let ((contributor-data (get-insurance-contributor user)))
    (and (>= (get cycles-contributed contributor-data) INSURANCE_ELIGIBILITY_CYCLES)
         (> (get eligible-since contributor-data) u0))))

(define-read-only (calculate-insurance-contribution (ubi-amount uint))
  (/ (* ubi-amount INSURANCE_CONTRIBUTION_RATE) u100))








  