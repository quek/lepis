(in-package :lepis)

(defvar *old-sigint-handler* nil)
(defvar *old-sigterm-handler* nil)
(defvar *old-sighup-handler* nil)

(defun signal-handler (&rest args)
  (sb-ext:with-locked-hash-table (*db-table*)
    (maphash (lambda (data-dir db)
               (format *standard-output* "~&start dump: ~a" data-dir)
               (dump-db db)
               (format *standard-output* "~&finished.")
               (ignore-errors (force-output *standard-input*)))
             *db-table*)))

(defmacro setup-signal-handler (old-handler-symbol signal)
  `(unless ,old-handler-symbol
     (setf ,old-handler-symbol
           (sb-sys:enable-interrupt
            ,signal
            (lambda (&rest args)
              (sb-sys:invoke-interruption
               (lambda ()
                 (sb-sys:with-interrupts
                   (apply 'signal-handler args)
                   (cond ((eq ,old-handler-symbol :default)
                          (sb-sys:default-interrupt ,signal)
                          (sb-posix:kill (sb-posix:getpid) ,signal))
                         ((functionp ,old-handler-symbol)
                          (apply ,old-handler-symbol args)))))))))))

(setup-signal-handler *old-sigint-handler* sb-unix:sigint)
(setup-signal-handler *old-sigterm-handler* sb-unix:sigterm)
(setup-signal-handler *old-sighup-handler* sb-unix:sighup)
