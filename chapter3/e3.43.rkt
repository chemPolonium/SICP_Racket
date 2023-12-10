#lang racket

; (serializer1 (serializer2 exchange)) means that both accounts are serialized. So each exchange will "lock" two account immediately and both accounts will not be released. Thus, after each exchange procedure, the accounts balances should be $10, $20, $30 in some order.
