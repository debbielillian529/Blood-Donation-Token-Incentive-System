(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-too-soon (err u104))

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
