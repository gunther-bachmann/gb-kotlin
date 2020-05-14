;;; gb-kotlin.el --- functions for kotlin projects

;; Copyright (C) 2020 Gunther Bachmann

;; Author: Gunther Bachmann <gunther.bachmann@web.de>
;; Keywords: kotlin
;; Version: 0.0.1
;; Created: 1st May 2020
;; Package-Requires: ((magit "2.90.1")(kotlin-mode "20191102.1510")(dash "20200426.2244")(helm "3.6.1")

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; functions for kotlin projects
;;

;; TODO: test that import 'as' is heeded!
;; TODO: does not check whether id is used within a string

;;; Code:
(require 'kotlin-mode)
(require 'magit)
(require 'dash)
(require 'helm)

(defun gb/kotlin--file-imports-all (file)
  "get all kotlin import lines for the given file"
  (split-string
   (s-trim
    (shell-command-to-string
     (s-concat (format "cat %s" file)
               " | grep -e '^import '"
               " | sort"
               " | uniq")))
   "\n"))

(defun gb/kotlin--import-line-to-standard-fqn-id-pair (import-line)
    "transform 'import <fqn> (as id)?' to (id . fqn)"
  (cond ((s-matches? "^import [^ ]+ as [^ ]+" import-line)
         (let* ((fqn-n-id (s-split " as " (s-chop-prefix "import " import-line)))
                (fqn (car fqn-n-id))
                (id (gb/kotlin--get-id-from-fqn fqn)))
           (cons id fqn)))
        ((s-matches? "^import [^ ]*" import-line)
         (let* ((fqn (car (s-split "[^a-ZA-Z_.]" (s-chop-prefix "import " import-line))))
                (id (gb/kotlin--get-id-from-fqn fqn)))
           (cons id fqn)))
        (t nil)))

(defun gb/kotlin--import-line-to-fqn-id-pair (import-line)
  "transform 'import <fqn> (as id)?' to (id . fqn)"
  (cond ((s-matches? "^import [^ ]+ +as +[^ ]+" import-line)
         (let* ((fqn-n-id (s-split " as " (s-chop-prefix "import " import-line)))
                (fqn (car fqn-n-id))
                (id (car (s-split "[^a-ZA-Z_]" (cadr fqn-n-id)))))
           (cons id fqn)))
        ((s-matches? "^import [^ ]*" import-line)
         (let* ((fqn (car (s-split "[^a-ZA-Z_.]" (s-chop-prefix "import " import-line))))
                (id (gb/kotlin--get-id-from-fqn fqn)))
           (cons id fqn)))
        (t nil)))

(defun gb/kotlin--gradle-cache-read-fqns ()
  "read all fqns (by class) from gradle cache"
  (let ((cmd "find ~/.gradle/caches/modules-2/ | grep -e \".*\\.jar$\" | xargs -i jar tf {} | grep  \"\\.class$\" | grep -v \"^META-INF\" | grep -v \"\\$[0-9]*\\.class$\" | grep -v \"\\$_\" | sed \"s/\\.class$//g\" | sed \"s|/|.|g\" | sed \"s|\\\\$|.|g\""))
    (async-shell-command cmd "*fqns*")
    ;; (s-split "\n" (s-trim (shell-command-to-string cmd )))
    ))

(defun gb/kotlin--gradle-cache-get-read-fqns ()
  "fetch the read fqns from the async buffer into a list of fqns"
  (s-split "\n" (with-current-buffer "*fqns*" (buffer-substring-no-properties (point-min) (point-max)))) )

(defun gb/kotlin--get-id-from-fqn (fqn)
  "split off last element of fqn"
  (car (reverse (s-split "\\." fqn))))

(defun gb/kotlin--project-imports-all (folder)
  "get all kotlin imports for the given project root (ignoring any 'as' renames)"
  (--map (gb/kotlin--import-line-to-standard-fqn-id-pair it)
        (split-string
         (s-trim
          (shell-command-to-string
           (format (s-concat "find %s -type f -name '*.kt' -exec cat {} \\;"
                             " | grep -e '^import '"
                             " | sort"
                             " | uniq")
                   folder)))
         "\n")))

(defun gb/kotlin--import-hash-set-of (class-package-pairs)
  "convert list of key value pairs of strings to a hash set that has either a string, or a list of strings as value"
  (let* ((import-hash (make-hash-table :test 'equal)))
    (mapc (lambda (it) (gb/kotlin--import-hash-add (car it) (cdr it) import-hash))
          class-package-pairs)
    import-hash))

(defun gb/kotlin--import-hash-add (key value hash)
  "add a key value pair to the import hash"
  (let* ((existing-value (gethash key hash))
         (unknown (not (gb/kotlin--import-hash-contains key value hash))))
    (cond ((not existing-value)
           (puthash key value hash))
          ((and unknown (listp existing-value))
           (puthash key (cons value existing-value) hash))
          (unknown
           (puthash key (list value existing-value) hash)
           ;; (message (format "multi list %s" key))
           )
          (t t)))
  hash)

(defun gb/kotlin--import-hash-contains (key value hash)
  "does the hash with import ids contain the given key value combination?"
  (let* ((existing-value (gethash key hash)))
    (or (and (listp existing-value)
          (--some (equal value it) existing-value))
       (equal value existing-value))))

;; (setq import-hash (gb/kotlin--import-hash-set-of (gb/kotlin--project-imports-all "~/repo/otto/eins/rule-processor")))

;; (gethash "DSL" import-hash)
;; (gethash "Query" import-hash)
;; (gethash "BLOCKING_PERMISSION" import-hash)
;; (gb/kotlin--import-hash-add "BLOCKING_PERMISSION" "some" import-hash)
;; (gb/kotlin--import-hash-contains "BLOCKING_PERMISSION" "somex" import-hash)

;; insert all xxx test imports into this buffer (execute via C-x C-e)
;; (--map (insert (format "\n(\"%s\" \"%s\")" (car (reverse (split-string it "\\."))) it)) (--filter it  (--map (when (s-contains? "spring" it) it) (-flatten (hash-table-values import-hash)))))


(defvar gb/kotlin--spring-imports
  (list
   '("SpringExtension" "org.springframework.test.context.junit.jupiter.SpringExtension")
   '("Service" "org.springframework.stereotype.Service")
   '("Component" "org.springframework.stereotype.Component")
   '("EmbeddedKafka" "org.springframework.kafka.test.context.EmbeddedKafka")
   '("EmbeddedKafkaBroker" "org.springframework.kafka.test.EmbeddedKafkaBroker")
   '("SendResult" "org.springframework.kafka.support.SendResult")
   '("KafkaTemplate" "org.springframework.kafka.core.KafkaTemplate")
   '("DefaultKafkaProducerFactory" "org.springframework.kafka.core.DefaultKafkaProducerFactory")
   '("Primary" "org.springframework.context.annotation.Primary")
   '("Import" "org.springframework.context.annotation.Import")
   '("ComponentScan" "org.springframework.context.annotation.ComponentScan")
   '("Bean" "org.springframework.context.annotation.Bean")
   '("ConfigurableApplicationContext" "org.springframework.context.ConfigurableApplicationContext")
   '("ApplicationContext" "org.springframework.context.ApplicationContext")
   '("TestConfiguration" "org.springframework.boot.test.context.TestConfiguration")
   '("SpringBootTest" "org.springframework.boot.test.context.SpringBootTest")
   '("runApplication" "org.springframework.boot.runApplication")
   '("EnableConfigurationProperties" "org.springframework.boot.context.properties.EnableConfigurationProperties")
   '("ConfigurationProperties" "org.springframework.boot.context.properties.ConfigurationProperties")
   '("DataSourceProperties" "org.springframework.boot.autoconfigure.jdbc.DataSourceProperties")
   '("GsonAutoConfiguration" "org.springframework.boot.autoconfigure.gson.GsonAutoConfiguration")
   '("FlywayAutoConfiguration" "org.springframework.boot.autoconfigure.flyway.FlywayAutoConfiguration")
   '("SpringBootApplication" "org.springframework.boot.autoconfigure.SpringBootApplication")
   '("SpringApplication" "org.springframework.boot.SpringApplication")
   '("Qualifier" "org.springframework.beans.factory.annotation.Qualifier")
   '("Autowired" "org.springframework.beans.factory.annotation.Autowired")
   '("Configuration" "org.springframework.context.annotation.Configuration"))
  "standard imports for spring")

(defvar gb/kotlin--kotlin-imports
  (list
   '("assertFailsWith" "kotlin.test.assertFailsWith")
   '("measureTimeMillis" "kotlin.system.measureTimeMillis")
   '("measureNanoTime" "kotlin.system.measureNanoTime")
   '("toList" "kotlin.streams.toList")
   '("Random" "kotlin.random.Random"))
  "standard import for kotlin")

(defvar gb/kotlin--flink-imports
  (list
   '("TypeExtractor" "org.apache.flink.api.java.typeutils.TypeExtractor")
   '("RowTypeInfo" "org.apache.flink.api.java.typeutils.RowTypeInfo")
   '("DataSource" "org.apache.flink.api.java.operators.DataSource")
   '("JDBCInputFormat" "org.apache.flink.api.java.io.jdbc.JDBCInputFormat")
   '("CollectionInputFormat" "org.apache.flink.api.java.io.CollectionInputFormat")
   '("Utils" "org.apache.flink.api.java.Utils")
   '("ExecutionEnvironment" "org.apache.flink.api.java.ExecutionEnvironment")
   '("DataSet" "org.apache.flink.api.java.DataSet"))
  "standard imports for flink")

(defvar gb/kotlin--java-stdlib-imports
  (list
   '("Stream" "java.util.stream.Stream")
   '("Collectors" "java.util.stream.Collectors")
   '("AtomicLong" "java.util.concurrent.atomic.AtomicLong")
   '("Executors" "java.util.concurrent.Executors")
   '("ExecutionException" "java.util.concurrent.ExecutionException")
   '("ConcurrentHashMap" "java.util.concurrent.ConcurrentHashMap")
   '("CompletableFuture" "java.util.concurrent.CompletableFuture")
   '("Properties" "java.util.Properties")
   '("Objects" "java.util.Objects")
   '("ArrayList" "java.util.ArrayList")
   '("MINUTES" "java.time.temporal.ChronoUnit.MINUTES")
   '("DateTimeFormatter" "java.time.format.DateTimeFormatter")
   '("LocalDateTime" "java.time.LocalDateTime")
   '("Types" "java.sql.Types")
   '("SQLFeatureNotSupportedException" "java.sql.SQLFeatureNotSupportedException")
   '("DriverManager" "java.sql.DriverManager")
   '("Connection" "java.sql.Connection")
   '("Security" "java.security.Security")
   '("Paths" "java.nio.file.Paths")
   '("URL" "java.net.URL")
   '("Serializable" "java.io.Serializable")
   '("File" "java.io.File")
   '("Closeable" "java.io.Closeable"))
  "standard imports for java")

(defvar gb/kotlin--assertj-imports
  (list
   '("Patch" "org.assertj.core.util.diff.Patch")
   '("DiffUtils" "org.assertj.core.util.diff.DiffUtils")
   '("ListAssert" "org.assertj.core.api.ListAssert")
   '("within" "org.assertj.core.api.Assertions.within")
   '("catchThrowable" "org.assertj.core.api.Assertions.catchThrowable")
   '("assertThatThrownBy" "org.assertj.core.api.Assertions.assertThatThrownBy")
   '("assertThat" "org.assertj.core.api.Assertions.assertThat")
   '("Assertions" "org.assertj.core.api.Assertions"))
  "standard imports for assertj")

(defvar gb/kotlin--mockk-imports
  (list
   '("verifyOrder" "io.mockk.verifyOrder")
   '("verify" "io.mockk.verify")
   '("mockkStatic" "io.mockk.mockkStatic")
   '("mockkConstructor" "io.mockk.mockkConstructor")
   '("mockk" "io.mockk.mockk")
   '("just" "io.mockk.just")
   '("every" "io.mockk.every")
   '("clearMocks" "io.mockk.clearMocks")
   '("Runs" "io.mockk.Runs"))
  "imports of mockk")

(defvar gb/kotlin--junit-jupiter-imports
  (list
   '("SpringExtension" "org.springframework.test.context.junit.jupiter.SpringExtension")
   '("RunWith" "org.junit.runner.RunWith")
   '("SuiteDisplayName" "org.junit.platform.suite.api.SuiteDisplayName")
   '("SelectPackages" "org.junit.platform.suite.api.SelectPackages")
   '("JUnitPlatform" "org.junit.platform.runner.JUnitPlatform")
   '("ClassRule" "org.junit.ClassRule")
   '("SpringExtension" "org.springframework.test.context.junit.jupiter.SpringExtension")
   '("ValueSource" "org.junit.jupiter.params.provider.ValueSource")
   '("MethodSource" "org.junit.jupiter.params.provider.MethodSource")
   '("EnumSource" "org.junit.jupiter.params.provider.EnumSource")
   '("Arguments" "org.junit.jupiter.params.provider.Arguments")
   '("ParameterizedTest" "org.junit.jupiter.params.ParameterizedTest")
   '("ExtensionContext" "org.junit.jupiter.api.extension.ExtensionContext")
   '("ExtendWith" "org.junit.jupiter.api.extension.ExtendWith")
   '("BeforeAllCallback" "org.junit.jupiter.api.extension.BeforeAllCallback")
   '("assertThrows" "org.junit.jupiter.api.assertThrows")
   '("assertDoesNotThrow" "org.junit.jupiter.api.assertDoesNotThrow")
   '("TestMethodOrder" "org.junit.jupiter.api.TestMethodOrder")
   '("TestInstance" "org.junit.jupiter.api.TestInstance")
   '("TestFactory" "org.junit.jupiter.api.TestFactory")
   '("Test" "org.junit.jupiter.api.Test")
   '("Order" "org.junit.jupiter.api.Order")
   '("Nested" "org.junit.jupiter.api.Nested")
   '("MethodOrderer" "org.junit.jupiter.api.MethodOrderer")
   '("dynamicTest" "org.junit.jupiter.api.DynamicTest.dynamicTest")
   '("Disabled" "org.junit.jupiter.api.Disabled")
   '("BeforeEach" "org.junit.jupiter.api.BeforeEach")
   '("BeforeAll" "org.junit.jupiter.api.BeforeAll")
   '("AfterEach" "org.junit.jupiter.api.AfterEach"))
  "all imports provided if junit / jupiter is wanted in the project")

(defun gb/kotlin--import-candidates-at-point ()
  "get import candidates for given word at point"
  (let* ((git-dir (magit-gitdir)))
    (when (and git-dir (eq 'kotlin-mode major-mode))
      (let* ((import-hash (gb/kotlin--import-hash-set-of
                           (gb/kotlin--project-imports-all
                            (format "%s/.." git-dir))))
             (word-ap (word-at-point)))
        (gethash word-ap import-hash)))))

(defun gb/kotlin--insert-on-first-import-line (fqn)
  "insert import for fqn at the first possible import position"
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^package .*")
    (if (search-forward-regexp "^import .*" nil t)
        (progn (move-beginning-of-line 1)
               (insert (format "import %s\n" fqn)))
      (insert (format "\n\nimport %s\n" fqn)))))

(defun gb/kotlin--insert-all-on-first-import-line (fqns)
  "insert all fqns on first possible import position"
  (--map (gb/kotlin--insert-on-first-import-line it) fqns))

(defun gb/kotlin--naive-check-use-of-fqn (fqn)
  "check whether this fqn is used"
  (let* ((id (car (reverse (split-string fqn "\\.")))))
    (gb/kotlin--naive-check-use-of-id id)))

(defun* gb/kotlin--naive-check-use-of-id (id)
  "check whether this id is used"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (format "[^a-bA-Z]\\(%s\\)[^a-zA-Z]" (regexp-quote id)) nil t)
      (goto-char (1- (match-end 1)))
      (when (and (s-contains? id (thing-at-point 'symbol t)) ;; not eq, since Collection<id> is returned, too
               (not (or (gb/kotlin--point-within-string)
                     (gb/kotlin--point-within-comment))))
        (return-from gb/kotlin--naive-check-use-of-id t)))))

(defun gb/kotlin--point-within-comment ()
  "return not nil if currently within a comment"
  (eq 'font-lock-comment-face (get-text-property (point) 'face)))

;; since kotlin mode currently has trouble to highlight function names enclosed in backquotes correctly, don't check
(defun gb/kotlin--point-within-string ()
  "return not nil if currently within a string"
  ;; (eq 'font-lock-string-face (get-text-property (point) 'face))
  nil)

(defun gb/kotlin--buffer-imports-all ()
  "read all imports from current selected buffer"
  (save-excursion
    (goto-char (point-min))
    (gb/kotlin--get-next-import-id-fqn-pairs)))

(defun gb/kotlin--get-next-import-id-fqn-pairs ()
  "get all fqns from point on by import instruction"
  (let* ((id-fqn-pairs nil))
    (while (search-forward-regexp "^import " nil t)
      (let* ((line (thing-at-point 'line t)))
        (setq id-fqn-pairs (cons (gb/kotlin--import-line-to-fqn-id-pair line) id-fqn-pairs))))
    id-fqn-pairs))


(defun gb/kotlin--get-buffer-package ()
  "get package fqn of current buffer"
  (save-excursion
    (goto-char (point-min))
    (when (word-search-forward "package" nil t)
      (let* ((package (s-match-strings-all "^package *\\(.*\\)" (thing-at-point 'line))))
        (substring-no-properties (cadar package))))))

(defun gb/kotlin--get-full-qualified-class-name-at-point ()
  "get the full qualified kotlin class name at point"
  (let ((class-name (substring-no-properties (word-at-point)))
        (package (gb/kotlin--get-buffer-package)))
    (when package
      (format "%s.%s" package class-name))))

(defun gb/kotlin--uniq-on-sorted-id-fqn-pairs (id-fqn-pairs)
  "when uniquifying, choose the more specific (the one with rename)"
  (let ((result (list)))
    (seq-doseq (elt id-fqn-pairs)
      (let* ((id (car elt))
             (fqn (cdr elt)))
        ;; (message (format "add import %s as %s to [ import %s as %s ] " fqn id (or (cdar result) "") (or (caar result) "")))
        (cond ((not result)
               (setq result (cons elt result)))
              ((and result
                  (not (string-equal fqn (cdar result))))
               (setq result (cons elt result)))
              ((and result
                  (string-equal fqn (cdar result))
                  (not (string-equal (caar result) id)))
               ;; (message "collision")
               (setq result (cons (if (string= id (gb/kotlin--get-id-from-fqn fqn))
                                      (car result)
                                    elt)
                                  (cdr result)))))))
    result))

(defun gb/kotlin--buffer-uniq-sorted-all-id-fqn-pairs ()
  "get all imports, sort and uniquify them"
  (gb/kotlin--uniq-on-sorted-id-fqn-pairs
   (sort
    (gb/kotlin--buffer-imports-all)
    (lambda (p1 p2)
      (let* ((p1-fqn (cdr p1))
             (p2-fqn (cdr p2)))
        (string< p2-fqn p1-fqn))))))

(defun gb/kotlin--insert-import-statement (id-fqn-pair)
  "insert (optionally with as) import statement"
  (if (equal (car id-fqn-pair) (gb/kotlin--get-id-from-fqn (cdr id-fqn-pair)))
      (insert (format "import %s\n" (cdr id-fqn-pair)))
    (insert (format "import %s as %s\n" (cdr id-fqn-pair) (car id-fqn-pair)))))

(defun gb/kotlin--remove-package-from-candidates (package candidates)
  "remove all candidates that share the given package"
  (let ((result (list)))
    (cond ((and candidates
              (listp candidates))
           (seq-doseq (elt candidates)
             (when (not (string= package (gb/kotlin--get-package-of-fqn elt)))
               (setq result (cons elt result)))))
          ((stringp candidates)
           (when (not (string= package (gb/kotlin--get-package-of-fqn candidates)))
             (setq result candidates)))
          (t (setq result nil)))
    result))

(defun gb/kotlin--get-package-of-fqn (fqn)
  "return the package part of a fqn"
  (s-chop-suffix (format ".%s" (gb/kotlin--get-id-from-fqn fqn)) fqn))

;; ================================================================================= API

(defun gb/kotlin-import-search-and-insert ()
  "search import in project and insert selected"
  (interactive)
  (if-let (git-dir (magit-gitdir))
      (let* ((import-hash (gb/kotlin--import-hash-set-of
                           (gb/kotlin--project-imports-all
                            (format "%s/.." git-dir))))
             (fqn (helm-comp-read "import:  " (-flatten (hash-table-values import-hash)) :must-match t)))
        (when fqn
          (gb/kotlin--insert-on-first-import-line fqn)))
    (message "function available only in git repository")))

(defun gb/kotlin-import-at-point ()
  "insert a kotlin import statement for the given"
  (interactive)
  (let* ((word-ap (word-at-point))
         (buffer-imports (gb/kotlin--import-hash-set-of (gb/kotlin--buffer-imports-all)))
         (package (gb/kotlin--get-buffer-package)))
    (if (gethash word-ap buffer-imports)
        (message (format "%s already known" word-ap))
      (let* ((candidates (gb/kotlin--remove-package-from-candidates package (gb/kotlin--import-candidates-at-point)))
             (fqn (cond ((and candidates (listp candidates))
                         (helm-comp-read "import: " candidates :must-match t))
                        (t candidates))))
        (if fqn
            (progn
              (gb/kotlin--insert-on-first-import-line fqn)
              (message (format "inserted 'import %s' at top" fqn)))
          (message (format "no candidates found for additional import of symbol '%s' at point" word-ap)))))))

(defun gb/kotlin-import-test-insert ()
  "insert all imports deemd handy, make sure to cleanup (e.g. via spotlessApply)"
  (interactive)
  (gb/kotlin--insert-all-on-first-import-line
   (--map (cadr it)
         (append gb/kotlin--mockk-imports
                 gb/kotlin--assertj-imports
                 gb/kotlin--junit-jupiter-imports))))

(defun gb/kotlin-import-stdlib-insert ()
  "insert all imports deemd handy, make sure to cleanup (e.g. via spotlessApply)"
  (interactive)
  (gb/kotlin--insert-all-on-first-import-line
   (--map (cadr it)
         (append gb/kotlin--kotlin-imports
                 gb/kotlin--java-stdlib-imports))))

(defun gb/kotlin-rewrite-import-region ()
  "rewrite import statements, sort, removing duplicates and (naive) checking symbol usage"
  (interactive)
  (if (eq 'kotlin-mode major-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((imported-id-fqn-pairs (gb/kotlin--buffer-uniq-sorted-all-id-fqn-pairs)))
          (search-forward-regexp "^import ")
          (move-beginning-of-line 1)
          (let* ((beg (point)))
            (goto-char (point-max))
            (search-backward-regexp "^import .*\n")
            (move-beginning-of-line 2)
            (let* ((end (point)))
              (delete-region beg end)
              (--map (gb/kotlin--insert-import-statement it)
                    (--filter (gb/kotlin--naive-check-use-of-fqn (car it)) imported-id-fqn-pairs))))))
    (message "NOT in kotlin mode")))

(defun gb/kotlin-kill-full-qualified-class-name-at-point ()
  "get the full qualified kotlin class name at point into the kill ring"
  (interactive)
  (let ((fqn (gb/kotlin--get-full-qualified-class-name-at-point)))
    (message fqn)
    (kill-new fqn)))

(provide 'gb-kotlin)
;;; gb-kotlin.el ends here
