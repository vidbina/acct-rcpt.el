(require 'ert)

(load-file "acct-rcpt.el")

(ert-deftest acct-rcpt--valid-context-label ()
;; make more robust by testing car and cdr
  (should (equal nil (car (acct-rcpt--valid-context-label 'two))))
  (should (equal nil (car (acct-rcpt--valid-context-label 12))))
  (should (equal nil (car (acct-rcpt--valid-context-label t))))
  (should (equal nil (car (acct-rcpt--valid-context-label " "))))
  (should (equal "Info" (car (acct-rcpt--valid-context-label "Info "))))
  (should (equal "$-sign" (car (acct-rcpt--valid-context-label "$-sign"))))
  (should (equal "Hi-Tech Corp" (car (acct-rcpt--valid-context-label "  Hi-Tech Corp  "))))
  (should (equal "Hüçø Co" (car (acct-rcpt--valid-context-label "Hüçø Co")))))

(ert-deftest acct-rcpt--valid-context-config ()
  (let ((dummy '((:base-directory . "/tmp/nice"))))
    (should (equal nil (car (acct-rcpt--valid-context-config " "))))
    (should (equal nil (car (acct-rcpt--valid-context-config '()))))
    (should (equal nil (car (acct-rcpt--valid-context-config '(:base-directory)))))
    (should (equal nil (car (acct-rcpt--valid-context-config '(:base-directory . "")))))
    (should (equal nil (car (acct-rcpt--valid-context-config '(:base-directory "hi")))))
    (should (equal nil (car (acct-rcpt--valid-context-config '((:base-directory "/tmp/nice"))))))
    (should (equal dummy (car (acct-rcpt--valid-context-config dummy))))
    (should (equal nil (car (acct-rcpt--valid-context-config '((:base-directory . :path))))))
    (should (equal nil (car (acct-rcpt--valid-context-config '((:base-directory))))))))

(ert-deftest acct-rcpt--valid-context ()
  ;; valid input
  (should (not (eq nil (car (acct-rcpt--valid-context (car
                                                       '(("Acme Corp" .
                                                          ((:base-directory . "/tmp/fin/Acme Corp")))
                                                         ("Muster GmbH" .
                                                          ((:base-directory . "/tmp/fin/Muster GmbH")))
                                                         ("Voorbeeld BV" .
                                                          ((:base-directory . "/tmp/blah/voorbeeld-bv"))))))))))
  (should (eq nil (cdr (acct-rcpt--valid-context (car
                                                  '(("Acme Corp" .
                                                     ((:base-directory . "/tmp/fin/Acme Corp")))
                                                    ("Muster GmbH" .
                                                     ((:base-directory . "/tmp/fin/Muster GmbH")))
                                                    ("Voorbeeld BV" .
                                                     ((:base-directory . "/tmp/blah/voorbeeld-bv")))))))))

  ;; invalid input
  (should (not (eq nil (cdr (acct-rcpt--valid-context '())))))
  (should (eq nil (car (acct-rcpt--valid-context '()))))

  (should (not (eq nil (cdr (acct-rcpt--valid-context 12)))))
  (should (eq nil (car (acct-rcpt--valid-context 12)))))

(ert-deftest acct-rcpt--valid-contexts ()
  ;; invalid contexts, check that car (key) is nil
  (should (eq nil (car (acct-rcpt--valid-contexts '()))))
  (should (eq nil (car (acct-rcpt--valid-contexts (list "accounting" "is" "not" "fun")))))
  (should (eq nil (car (acct-rcpt--valid-contexts 12))))
  ;; valid contexts, check that cdr (datum) is nil
  (should (eq nil (cdr
                   (acct-rcpt--valid-contexts
                    '(("Acme Corp" .
                       ((:base-directory . "/tmp/fin/Acme Corp")))
                      ("Muster GmbH" .
                       ((:base-directory . "/tmp/fin/Muster GmbH")))
                      ("Voorbeeld BV" .
                       ((:base-directory . "/tmp/blah/voorbeeld-bv")))))))))
