(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-too-soon (err u104))
(define-constant err-achievement-claimed (err u105))
(define-constant err-invalid-streak (err u106))
(define-constant err-donation-expired (err u107))
(define-constant err-invalid-referrer (err u108))
(define-constant err-self-referral (err u109))

(define-data-var token-name (string-ascii 32) "BloodToken")
(define-data-var token-symbol (string-ascii 10) "BLD")
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var tokens-per-donation uint u100)
(define-data-var donation-validity-period uint u172800)
(define-data-var referral-bonus-percentage uint u10)

(define-map donors
    principal
    {
        last-donation: uint,
        total-donations: uint,
        eligible: bool,
        achievements-claimed: (list 4 uint),
        current-streak: uint,
        longest-streak: uint,
        recent-donations: uint,
    }
)

(define-map balances
    principal
    uint
)

(define-map referrals
    principal
    {
        referrer: (optional principal),
        referral-count: uint,
        referral-rewards: uint,
    }
)

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri))
)

(define-read-only (get-balance (account principal))
    (ok (default-to u0 (map-get? balances account)))
)

(define-read-only (get-donor-info (donor principal))
    (ok (map-get? donors donor))
)

(define-read-only (get-referral-info (donor principal))
    (ok (map-get? referrals donor))
)

(define-read-only (get-referral-stats (donor principal))
    (match (map-get? referrals donor)
        referral-data (ok {
            referrer: (get referrer referral-data),
            referral-count: (get referral-count referral-data),
            referral-rewards: (get referral-rewards referral-data),
        })
        (ok {
            referrer: none,
            referral-count: u0,
            referral-rewards: u0,
        })
    )
)

(define-public (register-donor)
    (let ((donor tx-sender))
        (asserts! (is-none (map-get? donors donor)) err-already-registered)
        (map-set referrals donor {
            referrer: none,
            referral-count: u0,
            referral-rewards: u0,
        })
        (ok (map-set donors donor {
            last-donation: u0,
            total-donations: u0,
            eligible: true,
            achievements-claimed: (list),
            current-streak: u0,
            longest-streak: u0,
            recent-donations: u0,
        }))
    )
)

(define-public (register-donor-with-referrer (referrer-address principal))
    (let ((donor tx-sender))
        (asserts! (is-none (map-get? donors donor)) err-already-registered)
        (asserts! (not (is-eq donor referrer-address)) err-self-referral)
        (asserts! (is-some (map-get? donors referrer-address))
            err-invalid-referrer
        )
        (let ((referrer-data (unwrap! (map-get? referrals referrer-address) err-invalid-referrer)))
            (map-set referrals referrer-address {
                referrer: (get referrer referrer-data),
                referral-count: (+ (get referral-count referrer-data) u1),
                referral-rewards: (get referral-rewards referrer-data),
            })
        )
        (map-set referrals donor {
            referrer: (some referrer-address),
            referral-count: u0,
            referral-rewards: u0,
        })
        (ok (map-set donors donor {
            last-donation: u0,
            total-donations: u0,
            eligible: true,
            achievements-claimed: (list),
            current-streak: u0,
            longest-streak: u0,
            recent-donations: u0,
        }))
    )
)

(define-public (record-donation)
    (let (
            (donor tx-sender)
            (donor-info (unwrap! (map-get? donors donor) err-not-registered))
            (current-time burn-block-height)
            (cooling-period u8640)
        )
        (asserts!
            (>= (- current-time (get last-donation donor-info)) cooling-period)
            err-too-soon
        )
        (asserts! (get eligible donor-info) err-not-registered)
        (map-set balances donor
            (+ (default-to u0 (map-get? balances donor))
                (var-get tokens-per-donation)
            ))
        (match (map-get? referrals donor)
            referral-data (match (get referrer referral-data)
                referrer-address (let (
                        (referral-bonus (/
                            (* (var-get tokens-per-donation)
                                (var-get referral-bonus-percentage)
                            )
                            u100
                        ))
                        (ref-data (unwrap! (map-get? referrals referrer-address)
                            err-invalid-referrer
                        ))
                    )
                    (map-set balances referrer-address
                        (+ (default-to u0 (map-get? balances referrer-address))
                            referral-bonus
                        ))
                    (map-set referrals referrer-address {
                        referrer: (get referrer ref-data),
                        referral-count: (get referral-count ref-data),
                        referral-rewards: (+ (get referral-rewards ref-data) referral-bonus),
                    })
                )
                true
            )
            true
        )
        (let (
                (time-since-last (- current-time (get last-donation donor-info)))
                (streak-window u17280)
                (current-streak (get current-streak donor-info))
                (new-streak (if (<= time-since-last streak-window)
                    (+ current-streak u1)
                    u1
                ))
                (longest-streak (get longest-streak donor-info))
            )
            (ok (map-set donors donor {
                last-donation: current-time,
                total-donations: (+ (get total-donations donor-info) u1),
                eligible: true,
                achievements-claimed: (get achievements-claimed donor-info),
                current-streak: new-streak,
                longest-streak: (if (> new-streak longest-streak)
                    new-streak
                    longest-streak
                ),
                recent-donations: (+ (get recent-donations donor-info) u1),
            }))
        )
    )
)

(define-public (transfer
        (amount uint)
        (sender principal)
        (recipient principal)
    )
    (let ((sender-balance (default-to u0 (map-get? balances sender))))
        (asserts! (>= sender-balance amount) err-invalid-amount)
        (map-set balances sender (- sender-balance amount))
        (map-set balances recipient
            (+ (default-to u0 (map-get? balances recipient)) amount)
        )
        (ok true)
    )
)

(define-private (mint-tokens
        (recipient principal)
        (amount uint)
    )
    (ok (map-set balances recipient
        (+ (default-to u0 (map-get? balances recipient)) amount)
    ))
)

(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set token-uri new-uri))
    )
)

(define-public (update-tokens-per-donation (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set tokens-per-donation new-amount))
    )
)

(define-public (update-donation-validity-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set donation-validity-period new-period))
    )
)

(define-public (update-referral-bonus-percentage (new-percentage uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-percentage u100) err-invalid-amount)
        (ok (var-set referral-bonus-percentage new-percentage))
    )
)

(define-read-only (get-achievement-milestone (milestone uint))
    (if (is-eq milestone u5)
        u50
        (if (is-eq milestone u10)
            u150
            (if (is-eq milestone u25)
                u400
                (if (is-eq milestone u50)
                    u1000
                    u0
                )
            )
        )
    )
)

(define-read-only (is-milestone-eligible (donations uint))
    (or
        (is-eq donations u5)
        (is-eq donations u10)
        (is-eq donations u25)
        (is-eq donations u50)
    )
)

(define-read-only (has-claimed-achievement
        (donor principal)
        (milestone uint)
    )
    (match (map-get? donors donor)
        donor-data (is-some (index-of (get achievements-claimed donor-data) milestone))
        false
    )
)

(define-public (claim-achievement (milestone uint))
    (let (
            (donor tx-sender)
            (donor-info (unwrap! (map-get? donors donor) err-not-registered))
            (total-donations (get total-donations donor-info))
            (bonus-tokens (get-achievement-milestone milestone))
        )
        (asserts! (is-milestone-eligible milestone) err-invalid-amount)
        (asserts! (>= (get-valid-donation-count donor) milestone)
            err-donation-expired
        )
        (asserts! (not (has-claimed-achievement donor milestone))
            err-achievement-claimed
        )
        (map-set balances donor
            (+ (default-to u0 (map-get? balances donor)) bonus-tokens)
        )
        (ok (map-set donors donor {
            last-donation: (get last-donation donor-info),
            total-donations: total-donations,
            eligible: (get eligible donor-info),
            achievements-claimed: (unwrap!
                (as-max-len?
                    (append (get achievements-claimed donor-info) milestone)
                    u4
                )
                err-invalid-amount
            ),
            current-streak: (get current-streak donor-info),
            longest-streak: (get longest-streak donor-info),
            recent-donations: (get recent-donations donor-info),
        }))
    )
)

(define-read-only (is-donation-valid (donation-time uint))
    (let ((current-time burn-block-height))
        (< (- current-time donation-time) (var-get donation-validity-period))
    )
)

(define-read-only (get-valid-donation-count (donor principal))
    (match (map-get? donors donor)
        donor-data (let (
                (validity-period (var-get donation-validity-period))
                (current-time burn-block-height)
                (last-donation (get last-donation donor-data))
            )
            (if (is-donation-valid last-donation)
                (get recent-donations donor-data)
                u0
            )
        )
        u0
    )
)

(define-public (expire-old-donations (donor principal))
    (match (map-get? donors donor)
        donor-data (let (
                (current-time burn-block-height)
                (last-donation (get last-donation donor-data))
                (validity-period (var-get donation-validity-period))
            )
            (if (not (is-donation-valid last-donation))
                (ok (map-set donors donor {
                    last-donation: (get last-donation donor-data),
                    total-donations: (get total-donations donor-data),
                    eligible: (get eligible donor-data),
                    achievements-claimed: (get achievements-claimed donor-data),
                    current-streak: (get current-streak donor-data),
                    longest-streak: (get longest-streak donor-data),
                    recent-donations: u0,
                }))
                (ok true)
            )
        )
        err-not-registered
    )
)

(define-read-only (get-streak-bonus (streak uint))
    (if (>= streak u30)
        u1000
        (if (>= streak u20)
            u500
            (if (>= streak u10)
                u200
                (if (>= streak u5)
                    u50
                    u0
                )
            )
        )
    )
)

(define-read-only (is-streak-milestone (streak uint))
    (or
        (is-eq streak u5)
        (is-eq streak u10)
        (is-eq streak u20)
        (is-eq streak u30)
    )
)

(define-read-only (get-donor-streak (donor principal))
    (match (map-get? donors donor)
        donor-data
        {
            current-streak: (get current-streak donor-data),
            longest-streak: (get longest-streak donor-data),
        }
        {
            current-streak: u0,
            longest-streak: u0,
        }
    )
)

(define-public (claim-streak-bonus)
    (let (
            (donor tx-sender)
            (donor-info (unwrap! (map-get? donors donor) err-not-registered))
            (current-streak (get current-streak donor-info))
            (bonus-tokens (get-streak-bonus current-streak))
        )
        (asserts! (> bonus-tokens u0) err-invalid-streak)
        (asserts! (is-streak-milestone current-streak) err-invalid-streak)
        (map-set balances donor
            (+ (default-to u0 (map-get? balances donor)) bonus-tokens)
        )
        (ok true)
    )
)
