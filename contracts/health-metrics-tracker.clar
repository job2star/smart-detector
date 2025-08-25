;; health-metrics-tracker
;; 
;; A comprehensive smart contract for secure and decentralized health metrics tracking.
;; Enables users to record, monitor, and manage personal health data with granular control
;; and privacy-preserving mechanisms.

;; Error code constants for precise error handling
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-METRIC-TYPE (err u101))
(define-constant ERR-MEASUREMENT-INVALID (err u102))
(define-constant ERR-RECORD-NOT-FOUND (err u103))
(define-constant ERR-TIMESTAMP-INVALID (err u104))
(define-constant ERR-TIMEFRAME-INVALID (err u105))

;; Metric type enumeration for diverse health measurements
(define-constant METRIC-TYPE-PULSE u1)
(define-constant METRIC-TYPE-SYSTOLIC-BP u2)
(define-constant METRIC-TYPE-DIASTOLIC-BP u3)
(define-constant METRIC-TYPE-BLOOD-SUGAR u4)
(define-constant METRIC-TYPE-BODY-MASS u5)
(define-constant METRIC-TYPE-BODY-TEMP u6)
(define-constant METRIC-TYPE-OXYGEN-LEVEL u7)
(define-constant METRIC-TYPE-BREATH-RATE u8)

;; Data storage structures
(define-map health-measurements 
  { user: principal, timestamp: uint, metric-type: uint } 
  { value: uint, notes: (optional (string-utf8 256)) }
)

(define-map latest-measurement-timestamp
  { user: principal, metric-type: uint }
  { timestamp: uint }
)

(define-map measurement-count
  { user: principal, metric-type: uint }
  { count: uint }
)

;; Private validation functions

(define-private (is-valid-metric-type (metric-type uint))
  (or
    (is-eq metric-type METRIC-TYPE-PULSE)
    (is-eq metric-type METRIC-TYPE-SYSTOLIC-BP)
    (is-eq metric-type METRIC-TYPE-DIASTOLIC-BP)
    (is-eq metric-type METRIC-TYPE-BLOOD-SUGAR)
    (is-eq metric-type METRIC-TYPE-BODY-MASS)
    (is-eq metric-type METRIC-TYPE-BODY-TEMP)
    (is-eq metric-type METRIC-TYPE-OXYGEN-LEVEL)
    (is-eq metric-type METRIC-TYPE-BREATH-RATE)
  )
)

(define-private (is-valid-measurement (metric-type uint) (value uint))
  (if (is-eq metric-type METRIC-TYPE-PULSE)
      (and (>= value u30) (<= value u220))
    (if (is-eq metric-type METRIC-TYPE-SYSTOLIC-BP)
        (and (>= value u70) (<= value u250))
      (if (is-eq metric-type METRIC-TYPE-DIASTOLIC-BP)
          (and (>= value u40) (<= value u150))
        (if (is-eq metric-type METRIC-TYPE-BLOOD-SUGAR)
            (and (>= value u20) (<= value u600))
          (if (is-eq metric-type METRIC-TYPE-BODY-MASS)
              (and (>= value u1000) (<= value u500000))
            (if (is-eq metric-type METRIC-TYPE-BODY-TEMP)
                (and (>= value u340) (<= value u430))
              (if (is-eq metric-type METRIC-TYPE-OXYGEN-LEVEL)
                  (and (>= value u50) (<= value u100))
                (if (is-eq metric-type METRIC-TYPE-BREATH-RATE)
                    (and (>= value u4) (<= value u60))
                  false
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-private (update-recent-timestamp (user principal) (metric-type uint) (timestamp uint))
  (map-set latest-measurement-timestamp 
    { user: user, metric-type: metric-type }
    { timestamp: timestamp }
  )
)

(define-private (increment-metric-count (user principal) (metric-type uint))
  (let (
    (current-count (default-to u0 (get count (map-get? measurement-count { user: user, metric-type: metric-type }))))
  )
    (map-set measurement-count
      { user: user, metric-type: metric-type }
      { count: (+ current-count u1) }
    )
  )
)

;; Read-only query functions

(define-read-only (get-health-measurement (user principal) (timestamp uint) (metric-type uint))
  (map-get? health-measurements { user: user, timestamp: timestamp, metric-type: metric-type })
)

(define-read-only (get-latest-measurement (user principal) (metric-type uint))
  (let (
    (latest-timestamp (get timestamp (default-to { timestamp: u0 } 
                        (map-get? latest-measurement-timestamp { user: user, metric-type: metric-type }))))
  )
    (if (is-eq latest-timestamp u0)
        (ok none)
        (ok (map-get? health-measurements { user: user, timestamp: latest-timestamp, metric-type: metric-type }))
    )
  )
)

(define-read-only (get-metric-count (user principal) (metric-type uint))
  (default-to { count: u0 } (map-get? measurement-count { user: user, metric-type: metric-type }))
)

(define-read-only (check-metric-type-validity (metric-type uint))
  (ok (is-valid-metric-type metric-type))
)

(define-read-only (check-measurement-validity (metric-type uint) (value uint))
  (ok (is-valid-measurement metric-type value))
)

;; Public mutation functions

(define-public (record-health-metric (metric-type uint) (value uint) (timestamp uint) (notes (optional (string-utf8 256))))
  (let (
    (user tx-sender)
    (current-time (unwrap! (get-block-info? time (- block-height u1)) (err u500)))
  )
    ;; Validate inputs
    (asserts! (is-valid-metric-type metric-type) ERR-INVALID-METRIC-TYPE)
    (asserts! (is-valid-measurement metric-type value) ERR-MEASUREMENT-INVALID)
    (asserts! (<= timestamp current-time) ERR-TIMESTAMP-INVALID)
    
    ;; Store the health measurement
    (map-set health-measurements
      { user: user, timestamp: timestamp, metric-type: metric-type }
      { value: value, notes: notes }
    )
    
    ;; Update metadata
    (update-recent-timestamp user metric-type timestamp)
    (increment-metric-count user metric-type)
    
    (ok true)
  )
)

(define-public (update-health-metric (timestamp uint) (metric-type uint) (value uint) (notes (optional (string-utf8 256))))
  (let (
    (user tx-sender)
    (existing-record (map-get? health-measurements { user: user, timestamp: timestamp, metric-type: metric-type }))
  )
    ;; Validate inputs and state
    (asserts! (is-valid-metric-type metric-type) ERR-INVALID-METRIC-TYPE)
    (asserts! (is-valid-measurement metric-type value) ERR-MEASUREMENT-INVALID)
    (asserts! (is-some existing-record) ERR-RECORD-NOT-FOUND)
    
    ;; Update the record
    (map-set health-measurements
      { user: user, timestamp: timestamp, metric-type: metric-type }
      { value: value, notes: notes }
    )
    
    (ok true)
  )
)

(define-public (delete-health-metric (timestamp uint) (metric-type uint))
  (let (
    (user tx-sender)
    (existing-record (map-get? health-measurements { user: user, timestamp: timestamp, metric-type: metric-type }))
    (current-count (get count (default-to { count: u0 } (map-get? measurement-count { user: user, metric-type: metric-type }))))
  )
    ;; Validate state
    (asserts! (is-some existing-record) ERR-RECORD-NOT-FOUND)
    
    ;; Delete the record
    (map-delete health-measurements { user: user, timestamp: timestamp, metric-type: metric-type })
    
    ;; Update count
    (map-set measurement-count
      { user: user, metric-type: metric-type }
      { count: (- current-count u1) }
    )
    
    ;; Update latest timestamp if needed
    (let (
      (latest-timestamp (get timestamp (default-to { timestamp: u0 } 
                          (map-get? latest-measurement-timestamp { user: user, metric-type: metric-type }))))
    )
      (if (is-eq timestamp latest-timestamp)
          ;; Simplified timestamp management
          (map-delete latest-measurement-timestamp { user: user, metric-type: metric-type })
          true
      )
    )
    
    (ok true)
  )
)

(define-public (share-health-metric-with (recipient principal) (metric-type uint) (timestamp uint))
  (let (
    (user tx-sender)
    (health-data (map-get? health-measurements { user: user, timestamp: timestamp, metric-type: metric-type }))
  )
    (asserts! (is-some health-data) ERR-RECORD-NOT-FOUND)
    
    ;; Placeholder for more sophisticated sharing mechanism
    (ok health-data)
  )
)