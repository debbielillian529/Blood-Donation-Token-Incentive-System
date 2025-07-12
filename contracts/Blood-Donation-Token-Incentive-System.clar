(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-too-soon (err u104))
(define-constant err-achievement-claimed (err u105))

(define-data-var token-name (string-ascii 32) "BloodToken")
(define-data-var token-symbol (string-ascii 10) "BLD")
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var tokens-per-donation uint u100)

(define-map donors
    principal
    {
        last-donation: uint,
        total-donations: uint,
        eligible: bool,
        achievements-claimed: (list 4 uint),
    }
)

(define-map balances
    principal
    uint
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

(define-public (register-donor)
    (let ((donor tx-sender))
        (asserts! (is-none (map-get? donors donor)) err-already-registered)
        (ok (map-set donors donor {
            last-donation: u0,
            total-donations: u0,
            eligible: true,
            achievements-claimed: (list),
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
        (ok (map-set donors donor {
            last-donation: current-time,
            total-donations: (+ (get total-donations donor-info) u1),
            eligible: true,
            achievements-claimed: (get achievements-claimed donor-info),
        }))
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
        (asserts! (>= total-donations milestone) err-invalid-amount)
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
        }))
    )
)
