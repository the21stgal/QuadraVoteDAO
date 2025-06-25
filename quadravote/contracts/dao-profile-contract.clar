;; Quadratic Voting DAO
;; Implements quadratic voting where voting power = sqrt(token holdings)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-VOTING-PERIOD-ENDED (err u103))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u104))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u105))
(define-constant ERR-INSUFFICIENT-TOKENS (err u106))
(define-constant ERR-INVALID-PROPOSAL (err u107))
(define-constant ERR-PROPOSAL-EXPIRED (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VOTING-PERIOD u1440) ;; blocks (~10 days)
(define-constant MIN-PROPOSAL-THRESHOLD u1000) ;; minimum tokens to create proposal
(define-constant EXECUTION-DELAY u144) ;; blocks (~1 day)

;; Data Variables
(define-data-var proposal-counter uint u0)

;; Token balances map (since we can't dynamically call external contracts)
(define-map token-balances
  { account: principal }
  { balance: uint, last-updated: uint }
)

;; Proposal status enum
(define-constant PROPOSAL-PENDING u0)
(define-constant PROPOSAL-ACTIVE u1)
(define-constant PROPOSAL-PASSED u2)
(define-constant PROPOSAL-FAILED u3)
(define-constant PROPOSAL-EXECUTED u4)
(define-constant PROPOSAL-EXPIRED u5)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    start-block: uint,
    end-block: uint,
    for-votes: uint,
    against-votes: uint,
    status: uint,
    execution-block: uint,
    contract-call: (optional { contract: principal, function: (string-ascii 50), args: (list 10 uint) })
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool, ;; true = for, false = against
    weight: uint,
    block-height: uint
  }
)

(define-map member-info
  { member: principal }
  {
    total-votes-cast: uint,
    proposals-created: uint,
    last-activity: uint
  }
)

;; Helper Functions

;; Calculate square root using iterative approximation (Babylonian method)
(define-private (sqrt (n uint))
  (if (is-eq n u0)
    u0
    (if (<= n u1)
      u1
      (let 
        (
          (x0 (/ n u2))
          (x1 (/ (+ x0 (/ n x0)) u2))
          (x2 (/ (+ x1 (/ n x1)) u2))
          (x3 (/ (+ x2 (/ n x2)) u2))
          (x4 (/ (+ x3 (/ n x3)) u2))
          (x5 (/ (+ x4 (/ n x4)) u2))
        )
        x5 ;; 5 iterations should be sufficient for most cases
      )
    )
  )
)

;; Get token balance of a principal
(define-private (get-token-balance (account principal))
  (get balance 
    (default-to 
      { balance: u0, last-updated: u0 }
      (map-get? token-balances { account: account })
    )
  )
)

;; Calculate quadratic voting power
(define-private (calculate-voting-power (token-balance uint))
  (sqrt token-balance)
)

;; Check if proposal exists
(define-private (proposal-exists (proposal-id uint))
  (is-some (map-get? proposals { proposal-id: proposal-id }))
)

;; Get current block height
(define-private (get-block-height)
  block-height
)

;; Public Functions

;; Update token balance (called by token holders or authorized parties)
(define-public (update-token-balance (account principal) (balance uint))
  (begin
    ;; In a real implementation, you'd want additional authorization checks
    ;; This is a simplified version for demonstration
    (map-set token-balances
      { account: account }
      { balance: balance, last-updated: block-height }
    )
    (ok true)
  )
)

;; Batch update token balances (for efficiency)
(define-public (batch-update-balances (updates (list 100 { account: principal, balance: uint })))
  (begin
    (map update-single-balance updates)
    (ok (len updates))
  )
)

(define-private (update-single-balance (update { account: principal, balance: uint }))
  (map-set token-balances
    { account: (get account update) }
    { balance: (get balance update), last-updated: block-height }
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (contract-call-info (optional { contract: principal, function: (string-ascii 50), args: (list 10 uint) }))
)
  (let 
    (
      (proposer-balance (get-token-balance tx-sender))
      (proposal-id (+ (var-get proposal-counter) u1))
      (current-block (get-block-height))
    )
    ;; Check if proposer has enough tokens
    (asserts! (>= proposer-balance MIN-PROPOSAL-THRESHOLD) ERR-INSUFFICIENT-TOKENS)
    
    ;; Create proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-block: current-block,
        end-block: (+ current-block VOTING-PERIOD),
        for-votes: u0,
        against-votes: u0,
        status: PROPOSAL-ACTIVE,
        execution-block: (+ current-block VOTING-PERIOD EXECUTION-DELAY),
        contract-call: contract-call-info
      }
    )
    
    ;; Update proposal counter
    (var-set proposal-counter proposal-id)
    
    ;; Update member info
    (map-set member-info
      { member: tx-sender }
      (merge 
        (default-to 
          { total-votes-cast: u0, proposals-created: u0, last-activity: u0 }
          (map-get? member-info { member: tx-sender })
        )
        { proposals-created: (+ (get proposals-created 
                                  (default-to { total-votes-cast: u0, proposals-created: u0, last-activity: u0 }
                                             (map-get? member-info { member: tx-sender }))) u1),
          last-activity: current-block }
      )
    )
    
    (ok proposal-id)
  )
)

;; Cast a vote on a proposal
(define-public (vote (proposal-id uint) (vote-for bool))
  (let 
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (voter-balance (get-token-balance tx-sender))
      (voting-power (calculate-voting-power voter-balance))
      (current-block (get-block-height))
    )
    ;; Check if proposal is active
    (asserts! (is-eq (get status proposal) PROPOSAL-ACTIVE) ERR-PROPOSAL-NOT-ACTIVE)
    
    ;; Check if voting period is still active
    (asserts! (<= current-block (get end-block proposal)) ERR-VOTING-PERIOD-ENDED)
    
    ;; Check if voter hasn't already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Check if voter has tokens
    (asserts! (> voter-balance u0) ERR-INSUFFICIENT-TOKENS)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote-for,
        weight: voting-power,
        block-height: current-block
      }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal
        (if vote-for
          { for-votes: (+ (get for-votes proposal) voting-power), against-votes: (get against-votes proposal) }
          { for-votes: (get for-votes proposal), against-votes: (+ (get against-votes proposal) voting-power) }
        )
      )
    )
    
    ;; Update member info
    (map-set member-info
      { member: tx-sender }
      (merge 
        (default-to 
          { total-votes-cast: u0, proposals-created: u0, last-activity: u0 }
          (map-get? member-info { member: tx-sender })
        )
        { total-votes-cast: (+ (get total-votes-cast 
                                  (default-to { total-votes-cast: u0, proposals-created: u0, last-activity: u0 }
                                             (map-get? member-info { member: tx-sender }))) u1),
          last-activity: current-block }
      )
    )
    
    (ok voting-power)
  )
)

;; Finalize proposal after voting period
(define-public (finalize-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (current-block (get-block-height))
    )
    ;; Check if voting period has ended
    (asserts! (> current-block (get end-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Check if proposal is still active
    (asserts! (is-eq (get status proposal) PROPOSAL-ACTIVE) ERR-PROPOSAL-NOT-ACTIVE)
    
    ;; Determine result
    (let ((new-status (if (> (get for-votes proposal) (get against-votes proposal))
                        PROPOSAL-PASSED
                        PROPOSAL-FAILED)))
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: new-status })
      )
      (ok new-status)
    )
  )
)

;; Execute passed proposal (simplified - in practice would need more complex execution logic)
(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (current-block (get-block-height))
    )
    ;; Check if proposal passed
    (asserts! (is-eq (get status proposal) PROPOSAL-PASSED) ERR-PROPOSAL-NOT-ACTIVE)
    
    ;; Check if execution delay has passed
    (asserts! (>= current-block (get execution-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: PROPOSAL-EXECUTED })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get vote details
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Get member information
(define-read-only (get-member-info (member principal))
  (map-get? member-info { member: member })
)

;; Get voting power for an address
(define-read-only (get-voting-power (account principal))
  (let ((balance (get-token-balance account)))
    (calculate-voting-power balance)
  )
)

;; Get current proposal counter
(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

;; Get governance token balances
(define-read-only (get-token-balance-info (account principal))
  (map-get? token-balances { account: account })
)

;; Check if address can create proposal
(define-read-only (can-create-proposal (account principal))
  (>= (get-token-balance account) MIN-PROPOSAL-THRESHOLD)
)

;; Get proposal status
(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (ok (get status proposal))
    ERR-PROPOSAL-NOT-FOUND
  )
)

;; Get active proposals (helper function - returns proposal IDs that are active)
(define-read-only (is-proposal-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and 
               (is-eq (get status proposal) PROPOSAL-ACTIVE)
               (<= block-height (get end-block proposal)))
    false
  )
)
