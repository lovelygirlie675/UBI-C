;; UBI Skills Verification & Bonus System
;; Enables skill verification and skill-based bonuses for UBI recipients

;; Error constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-already-exists (err u402))
(define-constant err-unauthorized (err u403))
(define-constant err-invalid-data (err u404))
(define-constant err-insufficient-endorsements (err u405))
(define-constant err-skill-not-verified (err u406))
(define-constant err-already-endorsed (err u407))

;; Skill bonus constants
(define-constant basic-skill-bonus u50)
(define-constant advanced-skill-bonus u100)
(define-constant expert-skill-bonus u200)
(define-constant community-contribution-bonus u75)

;; Verification requirements
(define-constant min-endorsements-basic u3)
(define-constant min-endorsements-advanced u5)
(define-constant min-endorsements-expert u7)

;; Skill categories and verification levels
(define-map skill-categories
    { category-id: uint }
    {
        category-name: (string-ascii 50),
        description: (string-ascii 100),
        bonus-multiplier: uint,
        verification-required: bool,
        active: bool
    }
)

;; User skill registrations and verification status
(define-map user-skills
    { user: principal, skill-id: uint }
    {
        skill-category: uint,
        skill-description: (string-ascii 100),
        verification-level: (string-ascii 15),
        endorsement-count: uint,
        verified: bool,
        registered-at: uint,
        verified-at: uint,
        bonus-earned: uint
    }
)

;; Skill endorsements from other users
(define-map skill-endorsements
    { skill-owner: principal, skill-id: uint, endorser: principal }
    {
        endorsed: bool,
        endorsement-date: uint,
        endorser-reputation: uint,
        comment: (string-ascii 100)
    }
)

;; User skill statistics
(define-map user-skill-stats
    { user: principal }
    {
        total-skills: uint,
        verified-skills: uint,
        total-endorsements: uint,
        skill-bonus-earned: uint,
        reputation-score: uint
    }
)

;; Data variables for ID management
(define-data-var next-skill-category-id uint u1)
(define-data-var next-skill-id uint u1)

;; Create new skill category (admin only)
(define-public (create-skill-category
    (category-name (string-ascii 50))
    (description (string-ascii 100))
    (bonus-multiplier uint)
    (verification-required bool))
    (let
        ((category-id (var-get next-skill-category-id)))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> (len category-name) u0) err-invalid-data)
        (asserts! (<= bonus-multiplier u300) err-invalid-data)
        
        (map-set skill-categories
            { category-id: category-id }
            {
                category-name: category-name,
                description: description,
                bonus-multiplier: bonus-multiplier,
                verification-required: verification-required,
                active: true
            }
        )
        
        (var-set next-skill-category-id (+ category-id u1))
        (ok category-id)
    )
)

;; Register a skill for verification
(define-public (register-skill
    (skill-category uint)
    (skill-description (string-ascii 100))
    (verification-level (string-ascii 15)))
    (let
        ((skill-id (var-get next-skill-id))
         (category (unwrap! (map-get? skill-categories { category-id: skill-category }) err-not-found))
         (user-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                  skill-bonus-earned: u0, reputation-score: u0 }
                                (map-get? user-skill-stats { user: tx-sender }))))
        
        (asserts! (get active category) err-not-found)
        (asserts! (> (len skill-description) u0) err-invalid-data)
        (asserts! (or (is-eq verification-level "basic") 
                     (is-eq verification-level "advanced")
                     (is-eq verification-level "expert")) err-invalid-data)
        
        ;; Register the skill
        (map-set user-skills
            { user: tx-sender, skill-id: skill-id }
            {
                skill-category: skill-category,
                skill-description: skill-description,
                verification-level: verification-level,
                endorsement-count: u0,
                verified: false,
                registered-at: stacks-block-height,
                verified-at: u0,
                bonus-earned: u0
            }
        )
        
        ;; Update user statistics
        (map-set user-skill-stats
            { user: tx-sender }
            (merge user-stats { total-skills: (+ (get total-skills user-stats) u1) })
        )
        
        (var-set next-skill-id (+ skill-id u1))
        (ok skill-id)
    )
)

;; Endorse another user's skill
(define-public (endorse-skill
    (skill-owner principal)
    (skill-id uint)
    (comment (string-ascii 100)))
    (let
        ((skill (unwrap! (map-get? user-skills { user: skill-owner, skill-id: skill-id }) err-not-found))
         (existing-endorsement (map-get? skill-endorsements { skill-owner: skill-owner, skill-id: skill-id, endorser: tx-sender }))
         (endorser-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                      skill-bonus-earned: u0, reputation-score: u0 }
                                    (map-get? user-skill-stats { user: tx-sender })))
         (owner-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                   skill-bonus-earned: u0, reputation-score: u0 }
                                 (map-get? user-skill-stats { user: skill-owner }))))
        
        (asserts! (not (is-eq tx-sender skill-owner)) err-unauthorized)
        (asserts! (is-none existing-endorsement) err-already-endorsed)
        (asserts! (> (get reputation-score endorser-stats) u0) err-unauthorized)
        
        ;; Record endorsement
        (map-set skill-endorsements
            { skill-owner: skill-owner, skill-id: skill-id, endorser: tx-sender }
            {
                endorsed: true,
                endorsement-date: stacks-block-height,
                endorser-reputation: (get reputation-score endorser-stats),
                comment: comment
            }
        )
        
        ;; Update skill endorsement count
        (let ((new-endorsement-count (+ (get endorsement-count skill) u1)))
            (map-set user-skills
                { user: skill-owner, skill-id: skill-id }
                (merge skill { endorsement-count: new-endorsement-count })
            )
            
            ;; Check if skill should be verified
            (if (should-verify-skill (get verification-level skill) new-endorsement-count)
                (map-set user-skills
                    { user: skill-owner, skill-id: skill-id }
                    (merge skill { 
                        endorsement-count: new-endorsement-count,
                        verified: true,
                        verified-at: stacks-block-height
                    })
                )
                true
            )
        )
        
        ;; Update statistics
        (map-set user-skill-stats
            { user: skill-owner }
            (merge owner-stats { 
                total-endorsements: (+ (get total-endorsements owner-stats) u1),
                verified-skills: (if (should-verify-skill (get verification-level skill) 
                                                         (+ (get endorsement-count skill) u1))
                                    (+ (get verified-skills owner-stats) u1)
                                    (get verified-skills owner-stats))
            })
        )
        
        (ok true)
    )
)

;; Calculate skill bonus for UBI claim
(define-public (calculate-skill-bonus (user principal))
    (let
        ((user-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                  skill-bonus-earned: u0, reputation-score: u0 }
                                (map-get? user-skill-stats { user: user })))
         (verified-count (get verified-skills user-stats))
         (base-bonus (calculate-base-skill-bonus verified-count))
         (reputation-bonus (calculate-reputation-bonus (get reputation-score user-stats))))
        
        (let ((total-bonus (+ base-bonus reputation-bonus)))
            (map-set user-skill-stats
                { user: user }
                (merge user-stats { skill-bonus-earned: (+ (get skill-bonus-earned user-stats) total-bonus) })
            )
            
            (ok total-bonus)
        )
    )
)

;; Admin function to verify skill manually
(define-public (admin-verify-skill (skill-owner principal) (skill-id uint))
    (let
        ((skill (unwrap! (map-get? user-skills { user: skill-owner, skill-id: skill-id }) err-not-found)))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (get verified skill)) err-already-exists)
        
        (map-set user-skills
            { user: skill-owner, skill-id: skill-id }
            (merge skill {
                verified: true,
                verified-at: stacks-block-height
            })
        )
        
        (ok true)
    )
)

;; Update user reputation based on community feedback
(define-public (update-reputation (user principal) (reputation-change int))
    (let
        ((user-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                  skill-bonus-earned: u0, reputation-score: u50 }
                                (map-get? user-skill-stats { user: user })))
         (current-reputation (get reputation-score user-stats))
         (new-reputation (if (< reputation-change 0)
                           (if (> (to-uint (- 0 reputation-change)) current-reputation) u0
                               (- current-reputation (to-uint (- 0 reputation-change))))
                           (let ((increase (to-uint reputation-change)))
                               (if (> (+ current-reputation increase) u100)
                                   u100
                                   (+ current-reputation increase))))))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set user-skill-stats
            { user: user }
            (merge user-stats { reputation-score: new-reputation })
        )
        
        (ok new-reputation)
    )
)

;; Private helper functions
(define-private (should-verify-skill (level (string-ascii 15)) (endorsement-count uint))
    (if (is-eq level "basic") (>= endorsement-count min-endorsements-basic)
        (if (is-eq level "advanced") (>= endorsement-count min-endorsements-advanced)
            (if (is-eq level "expert") (>= endorsement-count min-endorsements-expert)
                false))))

(define-private (calculate-base-skill-bonus (verified-skills uint))
    (if (>= verified-skills u5) expert-skill-bonus
        (if (>= verified-skills u3) advanced-skill-bonus
            (if (>= verified-skills u1) basic-skill-bonus u0))))

(define-private (calculate-reputation-bonus (reputation uint))
    (if (>= reputation u90) u50
        (if (>= reputation u70) u30
            (if (>= reputation u50) u10 u0))))

;; Read-only functions
(define-read-only (get-skill-category (category-id uint))
    (map-get? skill-categories { category-id: category-id })
)

(define-read-only (get-user-skill (user principal) (skill-id uint))
    (map-get? user-skills { user: user, skill-id: skill-id })
)

(define-read-only (get-skill-endorsement (skill-owner principal) (skill-id uint) (endorser principal))
    (map-get? skill-endorsements { skill-owner: skill-owner, skill-id: skill-id, endorser: endorser })
)

(define-read-only (get-user-skill-stats (user principal))
    (map-get? user-skill-stats { user: user })
)

(define-read-only (get-skill-bonus-rate (user principal))
    (let
        ((user-stats (default-to { total-skills: u0, verified-skills: u0, total-endorsements: u0, 
                                  skill-bonus-earned: u0, reputation-score: u0 }
                                (map-get? user-skill-stats { user: user }))))
        
        {
            verified-skills: (get verified-skills user-stats),
            reputation: (get reputation-score user-stats),
            base-bonus: (calculate-base-skill-bonus (get verified-skills user-stats)),
            reputation-bonus: (calculate-reputation-bonus (get reputation-score user-stats)),
            total-bonus: (+ (calculate-base-skill-bonus (get verified-skills user-stats))
                           (calculate-reputation-bonus (get reputation-score user-stats)))
        }
    )
)

(define-read-only (get-skills-system-stats)
    {
        total-categories: (- (var-get next-skill-category-id) u1),
        total-skills: (- (var-get next-skill-id) u1),
        basic-bonus: basic-skill-bonus,
        advanced-bonus: advanced-skill-bonus,
        expert-bonus: expert-skill-bonus
    }
)
