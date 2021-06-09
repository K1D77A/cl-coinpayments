(in-package #:cl-coinpayments)

(defclass ipn-status ()
  ())

(defclass ipn-failure (ipn-status)
  ())

(defclass ipn-payment-pending (ipn-status)
  ())

(defclass ipn-payment-success (ipn-status)
  ())

(defclass negative-2 (ipn-failure)
  ()
  (:documentation "Paypal Refund or Reversal"))

(defclass negative-1 (ipn-failure)
  ()
  (:documentation "Cancelled/Timed out"))

(defclass zero (ipn-payment-pending)
  ()
  (:documentation "Waiting for buyer funds."))

(defclass one (ipn-payment-pending)
  ()
  (:documentation "We have confirmed coin reception from the buyer."))

(defclass two (ipn-payment-pending)
  ()
  (:documentation "queued for nightly payout. (if you have Payout Mode for this coin 
set to Nightly)"))

(defclass three (ipn-payment-pending)
  ()
  (:documentation "PayPal Pending (eChecks of other types of holds)"))

(defclass five (ipn-payment-pending)
  ()
  (:documentation "In Escrow (if you are using SetEscrow)"))

(defclass one-hundred (ipn-payment-success)
  ()
  (:documentation "Payment complete"))

;; (defclass coinpayment-ipn ()
;;   ((IPN-VERSION
;;     :documentation "1.0"
;;     :initarg :IPN-VERSION)
;;    (IPN-TYPE
;;     :documentation "Currently: 'simple, 'button', 'cart', 'donation', 'deposit', 'withdrawal', or 'api'"
;;     :initarg :IPN-TYPE)
;;    (IPN-MODE
;;     :documentation "Currently: 'hmac'"
;;     :initarg :IPN-MODE)
;;    (IPN-ID
;;     :documentation "The unique identifier of this IPN"
;;     :initarg :IPN-ID)
;;    (MERCHANT
;;     :documentation "Your merchant ID (you can find this on the My Account page)."
;;     :initarg :MERCHANT)
;;    (NOTE
;;     :documentation "Not listed on the api description but I just received it..."
;;     :initarg :NOTE)
;;    (FROM
;;     :documentation "Not listed in the api but its been received..."
;;     :initarg :FROM)))

;; (defclass ipn-deposit (coinpayment-ipn)
;;   ((TXN-ID
;;     :documentation "The coin transaction ID of the payment."
;;     :initarg :TXN-ID)
;;    (ADDRESS
;;     :documentation "Coin address the payment was received on."
;;     :initarg :ADDRESS)
;;    (DEST-TAG
;;     :documentation "For coins that use an extra tag it will include it here. For example Ripple Destination Tag, Monero Payment ID, etc."
;;     :initarg :DEST-TAG)
;;    (STATUS
;;     :documentation "Numeric status of the payment, currently 0 = pending and 100 = confirmed/complete. For future proofing you should use the same logic as Payment Statuses.
;; Dont release/ship product until the status is >=100 or queued for overnight payment.")
;;    (STATUS-TEXT
;;     :documentation "A text string describing the status of the payment. (useful for displaying in order comments)"
;;     :initarg :STATUS-TEXT)
;;    (CURRENCY
;;     :documentation "The coin the buyer paid with."
;;     :initarg :CURRENCY)
;;    (CONFIRMS
;;     :documentation "The number of confirms the payment has."
;;     :initarg :CONFIRMS)
;;    (AMOUNT
;;     :documentation "The total amount of the payment"
;;     :initarg :AMOUNT)
;;    (AMOUNTI
;;     :documentation "The total amount of the payment in Satoshis"
;;     :initarg :AMOUNTI)
;;    (FEE
;;     :documentation "The fee deducted by CoinPayments (only sent when status >= 100)"
;;     :initarg :FEE)
;;    (FEEI
;;     :documentation "The fee deducted by CoinPayments in Satoshis (only sent when status >= 100)"
;;     :initarg :FEEI)
;;    (FIAT-COIN
;;     :documentation "The ticker code of the fiat currency you selected on the Merchant Settings tab of the Account Settings page (USD, EUR, etc.) Make sure to check this for accuracy for security in your IPN handler!"
;;     :initarg :FIAT-COIN)
;;    (FIAT-AMOUNT
;;     :documentation "The total amount of the payment in the fiat currency you selected on the Merchant Settings tab of the Account Settings page."
;;     :initarg :FIAT-AMOUNT)
;;    (FIAT-AMOUNTI
;;     :documentation "The total amount of the payment in the fiat currency you selected in Satoshis"
;;     :initarg :FIAT-AMOUNTI)
;;    (FIAT-FEE
;;     :documentation "The fee deducted by CoinPayments in the fiat currency you selected (only sent when status >= 100)"
;;     :initarg :FIAT-FEE)
;;    (FIAT-FEEI
;;     :documentation "The fee deducted by CoinPayments in the fiat currency you selected in Satoshis (only sent when status >= 100)"
;;     :initarg :FIAT-FEEI)
;;    (LABEL
;;     :documentation "The address label if you have one set"
;;     :initarg :LABEL)))

;; (defclass ipn-withdrawal (coinpayment-ipn)
;;   ((ID
;;     :documentation "The ID of the withdrawal ('id' field returned from 'create_withdrawal'.)"
;;     :initarg :ID)
;;    (STATUS
;;     :documentation "Numeric status of the withdrawal, currently <0 = failed, 0 = waiting email confirmation, 1 = pending, and 2 = sent/complete."
;;     :initarg :STATUS)
;;    (STATUS-TEXT
;;     :documentation "A text string describing the status of the withdrawal."
;;     :initarg :STATUS-TEXT)
;;    (ADDRESS
;;     :documentation "Coin address the withdrawal was sent to."
;;     :initarg :ADDRESS)
;;    (TXN-ID
;;     :documentation "The coin transaction ID of the withdrawal."
;;     :initarg :TXN-ID)
;;    (CURRENCY
;;     :documentation "The coin of the withdrawal."
;;     :initarg :CURRENCY)
;;    (AMOUNT
;;     :documentation "The total amount of the withdrawal"
;;     :initarg :AMOUNT)
;;    (AMOUNTI
;;     :documentation "The total amount of the withdrawal in Satoshis"
;;     :initarg :AMOUNTI)))

;; (defclass ipn-buyer-information ()
;;   ((FIRST-NAME
;;     :documentation "Buyers first name"
;;     :initarg :FIRST-NAME)
;;    (LAST-NAME
;;     :documentation "Buyers last name"
;;     :initarg :LAST-NAME)
;;    (COMPANY
;;     :documentation "Buyer's company name."
;;     :initarg :COMPANY)
;;    (EMAIL
;;     :documentation "Buyer's email address."
;;     :initarg :EMAIL)))

;; (defclass ipn-shipping-information ()
;;   ((ADDRESS1
;;     :documentation "Street / address line 1"
;;     :initarg :ADDRESS1)
;;    (ADDRESS2
;;     :documentation "Street / address line 2"
;;     :initarg :ADDRESS2)
;;    (CITY
;;     :documentation "City"
;;     :initarg :CITY)
;;    (STATE
;;     :documentation "State / Province"
;;     :initarg :STATE)
;;    (ZIP
;;     :documentation "Zip / Postal Code"
;;     :initarg :ZIP)
;;    (COUNTRY
;;     :documentation "Country of residence IS03166"
;;     :initarg :COUNTRY)
;;    (COUNTRY-NAME
;;     :documentation "Pretty version of COUNTRY"
;;     :initarg :COUNTRY-NAME)
;;    (PHONE
;;     :documentation "Phone number"
;;     :initarg :PHONE)))

;; (defclass ipn-simple-button (coinpayment-ipn ipn-shipping-information ipn-buyer-information)
;;   ((STATUS
;;     :documentation "The status of the payment. See Payment Statuses for details."
;;     :initarg :STATUS)
;;    (STATUS-TEXT
;;     :documentation "A text string describing the status of the payment. 
;; (useful for displaying in order comments)"
;;     :initarg :STATUS-TEXT)
;;    (TXN-ID
;;     :documentation "Unique ID of the payment"
;;     :initarg :TXN-ID)
;;    (CURRENCY1
;;     :documentation "The original currency/coin submitting in your button"
;;     :initarg :CURRENCY1)
;;    (CURRENCY2
;;     :documentation "The coin the buyer chose to pay with."
;;     :initarg :CURRENCY2)
;;    (AMOUNT1
;;     :documentation "The total amount of the payment in your original currency/coin."
;;     :initarg :AMOUNT1)
;;    (AMOUNT2
;;     :documentation "The total amount of the payment in the buyer's selected coin."
;;     :initarg :AMOUNT2)
;;    (SUBTOTAL
;;     :documentation "The subtotal of the order before shipping and tax in your original currency/coin."
;;     :initarg :SUBTOTAL)
;;    (SHIPPING
;;     :documentation "The shipping charged on the order in your original currency/coin."
;;     :initarg :SHIPPING)
;;    (TAX
;;     :documentation "The tax on the order in your original currency/coin."
;;     :initarg :TAX)
;;    (FEE
;;     :documentation "The fee on the payment in the buyer's selected coin."
;;     :initarg :FEE)
;;    (NET
;;     :documentation "The net amount you received of the buyer's selected coin after our fee and any coin TX fees to send the coins to you."
;;     :initarg :NET)
;;    (ITEM-AMOUNT
;;     :documentation "The amount of the item/order in the original currency/coin."
;;     :initarg :ITEM-AMOUNT)
;;    (ITEM-NAME
;;     :documentation "The name of the item that was purchased."
;;     :initarg :ITEM-NAME)
;;    (ITEM-DESC
;;     :documentation "Description of the item that was purchased."
;;     :initarg :ITEM-DESC)
;;    (ITEM-NUMBER
;;     :documentation "This is a passthru variable for your own use. [visible to buyer]"
;;     :initarg :ITEM-NUMBER)
;;    (INVOICE
;;     :documentation "This is a passthru variable for your own use. [not visible to buyer]"
;;     :initarg :INVOICE)
;;    (CUSTOM
;;     :documentation "This is a passthru variable for your own use. [not visible to buyer]"
;;     :initarg :CUSTOM)
;;    (ON1
;;     :documentation "1st option name."
;;     :initarg :ON1)
;;    (OV1
;;     :documentation "1st option value."
;;     :initarg :OV1)
;;    (ON2
;;     :documentation "2nd option name."
;;     :initarg :ON2)
;;    (OV2
;;     :documentation "2nd option value."
;;     :initarg :OV2)
;;    (SEND-TX
;;     :documentation "The TX ID of the payment to the merchant. Only included when 'status' >= 100 and if the payment mode is set to ASAP or Nightly or if the payment is PayPal Passthru."
;;     :initarg :SEND-TX)
;;    (RECEIVED-AMOUNT
;;     :documentation "The amount of currency2 received at the time the IPN was generated."
;;     :initarg :RECEIVED-AMOUNT)
;;    (RECEIVED-CONFIRMS
;;     :documentation "The number of confirms of 'received_amount' at the time the IPN was generated."
;;     :initarg :RECEIVED-CONFIRMS)))

;; (defclass ipn-advanced-button (coinpayment-ipn ipn-shipping-information ipn-buyer-information)
;;   ((STATUS
;;     :documentation "The status of the payment. See Payment Statuses for details."
;;     :initarg :STATUS)
;;    (STATUS-TEXT
;;     :documentation "A text string describing the status of the payment. (useful for displaying in order comments)"
;;     :initarg :STATUS-TEXT)
;;    (TXN-ID
;;     :documentation "Unique ID of the payment"
;;     :initarg :TXN-ID)
;;    (CURRENCY1
;;     :documentation "The original currency/coin submitting in your button"
;;     :initarg :CURRENCY1)
;;    (CURRENCY2
;;     :documentation "The coin the buyer chose to pay with."
;;     :initarg :CURRENCY2)
;;    (AMOUNT1
;;     :documentation "The total amount of the payment in your original currency/coin."
;;     :initarg :AMOUNT1)
;;    (AMOUNT2
;;     :documentation "The total amount of the payment in the buyer's selected coin."
;;     :initarg :AMOUNT2)
;;    (SUBTOTAL
;;     :documentation "The subtotal of the order before shipping and tax in your original currency/coin."
;;     :initarg :SUBTOTAL)
;;    (SHIPPING
;;     :documentation "The shipping charged on the order in your original currency/coin."
;;     :initarg :SHIPPING)
;;    (TAX
;;     :documentation "The tax on the order in your original currency/coin."
;;     :initarg :TAX)
;;    (FEE
;;     :documentation "The fee on the payment in the buyer's selected coin."
;;     :initarg :FEE)
;;    (NET
;;     :documentation "The net amount you received of the buyer's selected coin after our fee and any coin TX fees to send the coins to you."
;;     :initarg :NET)
;;    (ITEM-AMOUNT
;;     :documentation "The amount per-item in the original currency/coin."
;;     :initarg :ITEM-AMOUNT)
;;    (ITEM-NAME
;;     :documentation "The name of the item that was purchased."
;;     :initarg :ITEM-NAME)
;;    (QUANTITY
;;     :documentation "The quantity of items bought."
;;     :initarg :QUANTITY)
;;    (ITEM-NUMBER
;;     :documentation "This is a passthru variable for your own use. [visible to buyer]"
;;     :initarg :ITEM-NUMBER)
;;    (INVOICE
;;     :documentation "This is a passthru variable for your own use. [not visible to buyer]"
;;     :initarg :INVOICE)
;;    (CUSTOM
;;     :documentation "This is a passthru variable for your own use. [not visible to buyer]"
;;     :initarg :CUSTOM)
;;    (ON1
;;     :documentation "1st option name."
;;     :initarg :ON1)
;;    (OV1
;;     :documentation "1st option value."
;;     :initarg :OV1)
;;    (ON2
;;     :documentation "2nd option name."
;;     :initarg :ON2)
;;    (OV2
;;     :documentation "2nd option value."
;;     :initarg :OV2)
;;    (EXTRA
;;     :documentation "A note from the buyer."
;;     :initarg :EXTRA)
;;    (SEND-TX
;;     :documentation "The TX ID of the payment to the merchant. Only included when 'status' >= 100 and if the payment mode is set to ASAP or Nightly or if the payment is PayPal Passthru."
;;     :initarg :SEND-TX)
;;    (RECEIVED-AMOUNT
;;     :documentation "The amount of currency2 received at the time the IPN was generated."
;;     :initarg :RECEIVED-AMOUNT)
;;    (RECEIVED-CONFIRMS
;;     :documentation "The number of confirms of 'received_amount' at the time the IPN was generated."
;;     :initarg :RECEIVED-CONFIRMS)))

;; (defclass ipn-donation (coinpayment-ipn ipn-shipping-information ipn-buyer-information)
;;   ((STATUS
;;     :documentation "The status of the payment. See Payment Statuses for details."
;;     :initarg :STATUS)
;;    (STATUS-TEXT
;;     :documentation "A text string describing the status of the payment. (useful for displaying in order comments)"
;;     :initarg :STATUS-TEXT)
;;    (TXN-ID
;;     :documentation "Unique ID of the payment"
;;     :initarg :TXN-ID)
;;    (CURRENCY2
;;     :documentation "The coin the donator chose to pay with."
;;     :initarg :CURRENCY2)
;;    (AMOUNT1
;;     :documentation "The total amount of the payment in your original currency/coin."
;;     :initarg :AMOUNT1)
;;    (AMOUNT2
;;     :documentation "The total amount of the payment in the donator's selected coin."
;;     :initarg :AMOUNT2)
;;    (SUBTOTAL
;;     :documentation "The subtotal of the order before shipping and tax in your original currency/coin."
;;     :initarg :SUBTOTAL)
;;    (SHIPPING
;;     :documentation "The shipping charged on the order in your original currency/coin."
;;     :initarg :SHIPPING)
;;    (TAX
;;     :documentation "The tax on the order in your original currency/coin."
;;     :initarg :TAX)
;;    (FEE
;;     :documentation "The fee on the payment in the donator's selected coin."
;;     :initarg :FEE)
;;    (NET
;;     :documentation "The net amount you received of the buyer's selected coin after our fee and any coin TX fees to send the coins to you."
;;     :initarg :NET)
;;    (ITEM-NAME
;;     :documentation "The name of the donation."
;;     :initarg :ITEM-NAME)
;;    (ITEM-NUMBER
;;     :documentation "This is a passthru variable for your own use. [not visible to donator]"
;;     :initarg :ITEM-NUMBER)
;;    (INVOICE
;;     :documentation "This is a passthru variable for your own use. [not visible to donator]"
;;     :initarg :INVOICE)
;;    (CUSTOM
;;     :documentation "This is a passthru variable for your own use. [not visible to donator]"
;;     :initarg :CUSTOM)
;;    (ON1
;;     :documentation "1st option name."
;;     :initarg :ON1)
;;    (OV1
;;     :documentation "1st option value."
;;     :initarg :OV1)
;;    (ON2
;;     :documentation "2nd option name."
;;     :initarg :ON2)
;;    (OV2
;;     :documentation "2nd option value."
;;     :initarg :OV2)
;;    (EXTRA
;;     :documentation "A note from the buyer."
;;     :initarg :EXTRA)
;;    (SEND-TX
;;     :documentation "The TX ID of the payment to the merchant. Only included when 'status' >= 100 and if the payment mode is set to ASAP or Nightly or if the payment is PayPal Passthru."
;;     :initarg :SEND-TX)
;;    (RECEIVED-AMOUNT
;;     :documentation "The amount of currency2 received at the time the IPN was generated."
;;     :initarg :RECEIVED-AMOUNT)
;;    (RECEIVED-CONFIRMS
;;     :documentation "The number of confirms of 'received_amount' at the time the IPN was generated."
;;     :initarg :RECEIVED-CONFIRMS)))

;; (defclass ipn-unknown ()
;;   ((plist
;;     :accessor plist
;;     :initarg :plist))
;;   (:documentation "Instantiated when the type is unknown."))
