;; TrackTideShip - AI-Powered Predictive Quality Assurance Platform
;; A blockchain-based supply chain quality tracking system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-quality (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-batch-sealed (err u106))

;; Minimum stake required to become a validator (in microSTX)
(define-constant min-validator-stake u1000000)

;; Quality threshold (0-100 scale)
(define-constant quality-threshold u70)

;; Data Variables
(define-data-var batch-nonce uint u0)
(define-data-var validator-nonce uint u0)

;; Data Maps

;; Product Batch Tracking
(define-map product-batches
    { batch-id: uint }
    {
        manufacturer: principal,
        product-name: (string-ascii 50),
        timestamp: uint,
        temperature: int,
        humidity: uint,
        shock-impact: uint,
        chemical-exposure: uint,
        quality-score: uint,
        status: (string-ascii 20),
        location: (string-ascii 100),
        sealed: bool
    }
)

;; Quality History for each batch
(define-map quality-records
    { batch-id: uint, record-id: uint }
    {
        timestamp: uint,
        temperature: int,
        humidity: uint,
        shock-impact: uint,
        chemical-exposure: uint,
        quality-score: uint,
        validator: principal
    }
)

;; Track number of quality records per batch
(define-map batch-record-count
    { batch-id: uint }
    { count: uint }
)

;; Validator Registry
(define-map validators
    { validator: principal }
    {
        stake-amount: uint,
        accuracy-score: uint,
        total-validations: uint,
        active: bool,
        registration-time: uint
    }
)

;; Quality Alerts
(define-map quality-alerts
    { batch-id: uint }
    {
        alert-type: (string-ascii 30),
        timestamp: uint,
        triggered-by: principal,
        resolved: bool
    }
)

;; Read-only functions

(define-read-only (get-batch-info (batch-id uint))
    (map-get? product-batches { batch-id: batch-id })
)

(define-read-only (get-quality-record (batch-id uint) (record-id uint))
    (map-get? quality-records { batch-id: batch-id, record-id: record-id })
)

(define-read-only (get-validator-info (validator principal))
    (map-get? validators { validator: validator })
)

(define-read-only (get-batch-record-count (batch-id uint))
    (default-to { count: u0 } (map-get? batch-record-count { batch-id: batch-id }))
)

(define-read-only (get-quality-alert (batch-id uint))
    (map-get? quality-alerts { batch-id: batch-id })
)

(define-read-only (get-current-batch-nonce)
    (var-get batch-nonce)
)

(define-read-only (is-validator-active (validator principal))
    (match (map-get? validators { validator: validator })
        validator-info (get active validator-info)
        false
    )
)

;; Public functions

;; Register as a validator with stake
(define-public (register-validator (stake-amount uint))
    (let
        (
            (validator tx-sender)
        )
        (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
        (asserts! (is-none (map-get? validators { validator: validator })) err-already-exists)
        
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        (ok (map-set validators
            { validator: validator }
            {
                stake-amount: stake-amount,
                accuracy-score: u100,
                total-validations: u0,
                active: true,
                registration-time: block-height
            }
        ))
    )
)

;; Create a new product batch
(define-public (create-batch 
    (product-name (string-ascii 50))
    (temperature int)
    (humidity uint)
    (location (string-ascii 100)))
    (let
        (
            (new-batch-id (+ (var-get batch-nonce) u1))
            (initial-quality u100)
        )
        (var-set batch-nonce new-batch-id)
        
        (map-set product-batches
            { batch-id: new-batch-id }
            {
                manufacturer: tx-sender,
                product-name: product-name,
                timestamp: block-height,
                temperature: temperature,
                humidity: humidity,
                shock-impact: u0,
                chemical-exposure: u0,
                quality-score: initial-quality,
                status: "active",
                location: location,
                sealed: false
            }
        )
        
        (map-set batch-record-count
            { batch-id: new-batch-id }
            { count: u0 }
        )
        
        (ok new-batch-id)
    )
)

;; Record quality data (only by active validators)
(define-public (record-quality-data
    (batch-id uint)
    (temperature int)
    (humidity uint)
    (shock-impact uint)
    (chemical-exposure uint))
    (let
        (
            (batch (unwrap! (map-get? product-batches { batch-id: batch-id }) err-not-found))
            (validator-info (unwrap! (map-get? validators { validator: tx-sender }) err-unauthorized))
            (record-count (get count (get-batch-record-count batch-id)))
            (new-record-id (+ record-count u1))
            (quality-score (calculate-quality-score temperature humidity shock-impact chemical-exposure))
        )
        (asserts! (get active validator-info) err-unauthorized)
        (asserts! (not (get sealed batch)) err-batch-sealed)
        
        ;; Record quality data
        (map-set quality-records
            { batch-id: batch-id, record-id: new-record-id }
            {
                timestamp: block-height,
                temperature: temperature,
                humidity: humidity,
                shock-impact: shock-impact,
                chemical-exposure: chemical-exposure,
                quality-score: quality-score,
                validator: tx-sender
            }
        )
        
        ;; Update record count
        (map-set batch-record-count
            { batch-id: batch-id }
            { count: new-record-id }
        )
        
        ;; Update batch with latest quality data
        (map-set product-batches
            { batch-id: batch-id }
            (merge batch {
                temperature: temperature,
                humidity: humidity,
                shock-impact: shock-impact,
                chemical-exposure: chemical-exposure,
                quality-score: quality-score
            })
        )
        
        ;; Update validator stats
        (map-set validators
            { validator: tx-sender }
            (merge validator-info {
                total-validations: (+ (get total-validations validator-info) u1)
            })
        )
        
        ;; Trigger alert if quality below threshold
        (if (< quality-score quality-threshold)
            (begin
                (map-set quality-alerts
                    { batch-id: batch-id }
                    {
                        alert-type: "quality-threshold-breach",
                        timestamp: block-height,
                        triggered-by: tx-sender,
                        resolved: false
                    }
                )
                (ok { batch-id: batch-id, quality-score: quality-score, alert: true })
            )
            (ok { batch-id: batch-id, quality-score: quality-score, alert: false })
        )
    )
)

;; Update batch status
(define-public (update-batch-status (batch-id uint) (new-status (string-ascii 20)))
    (let
        (
            (batch (unwrap! (map-get? product-batches { batch-id: batch-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get manufacturer batch)) err-unauthorized)
        (asserts! (not (get sealed batch)) err-batch-sealed)
        
        (ok (map-set product-batches
            { batch-id: batch-id }
            (merge batch { status: new-status })
        ))
    )
)

;; Seal a batch (final certification)
(define-public (seal-batch (batch-id uint))
    (let
        (
            (batch (unwrap! (map-get? product-batches { batch-id: batch-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get manufacturer batch)) err-unauthorized)
        (asserts! (not (get sealed batch)) err-batch-sealed)
        
        (ok (map-set product-batches
            { batch-id: batch-id }
            (merge batch { sealed: true })
        ))
    )
)

;; Resolve quality alert
(define-public (resolve-alert (batch-id uint))
    (let
        (
            (alert (unwrap! (map-get? quality-alerts { batch-id: batch-id }) err-not-found))
            (batch (unwrap! (map-get? product-batches { batch-id: batch-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get manufacturer batch)) err-unauthorized)
        
        (ok (map-set quality-alerts
            { batch-id: batch-id }
            (merge alert { resolved: true })
        ))
    )
)

;; Update validator accuracy score (contract owner only)
(define-public (update-validator-accuracy (validator principal) (new-accuracy uint))
    (let
        (
            (validator-info (unwrap! (map-get? validators { validator: validator }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-accuracy u100) err-invalid-quality)
        
        (ok (map-set validators
            { validator: validator }
            (merge validator-info { accuracy-score: new-accuracy })
        ))
    )
)

;; Deactivate validator (contract owner only)
(define-public (deactivate-validator (validator principal))
    (let
        (
            (validator-info (unwrap! (map-get? validators { validator: validator }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (ok (map-set validators
            { validator: validator }
            (merge validator-info { active: false })
        ))
    )
)

;; Private functions

;; Calculate quality score based on sensor data
(define-private (calculate-quality-score 
    (temperature int)
    (humidity uint)
    (shock-impact uint)
    (chemical-exposure uint))
    (let
        (
            ;; Base quality score
            (base-score u100)
            
            ;; Temperature penalty (assuming optimal range -5 to 25)
            (temp-penalty (if (or (< temperature -5) (> temperature 25)) u15 u0))
            
            ;; Humidity penalty (assuming optimal range 30-60%)
            (humidity-penalty (if (or (< humidity u30) (> humidity u60)) u10 u0))
            
            ;; Shock impact penalty
            (shock-penalty (if (> shock-impact u50) u20 u0))
            
            ;; Chemical exposure penalty
            (chemical-penalty (if (> chemical-exposure u30) u25 u0))
            
            ;; Total penalty
            (total-penalty (+ temp-penalty (+ humidity-penalty (+ shock-penalty chemical-penalty))))
        )
        (if (> total-penalty base-score)
            u0
            (- base-score total-penalty)
        )
    )
)