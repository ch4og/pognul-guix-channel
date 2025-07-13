;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pognul packages ksh)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages commencement))

(define-public ksh
  (let ((commit "11981f5fd6d6d6cb758ddf176b47126162daa618")
        (version "1.0.10")
        (revision "1")
        (release? #f))
    (package
      (name "ksh")
      (version (if release? version
                   (git-version version revision commit)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ksh93/ksh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10r0h2i1n6zh6s0n4pgkv6z44ld77im2ma7p9i6272r0v6jr8d9y"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'build 'patch-mamfiles
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (use-modules (guix build utils))
                (substitute* "src/cmd/ksh93/Mamfile"
                  (("mv -f git.h.new git.h")
                   (string-append "\n\t\texec -\techo \"#define git_commit   \\\""
                                  (string-take #$commit 8) "\\\"\" > git.h")))
                (when (not #$release?)
                  (substitute* "src/lib/libast/Mamfile"
                    (("mv -f ast_release.h.new ast_release.h")
                     (string-append "\n"
                      "\t\texec -\techo \"#undef _AST_release\" > ast_release.h"))))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (setenv "CC" (which "gcc"))
                (setenv "AR" (which "ar"))
                (setenv "LD" (which "ld"))
                (setenv "NM" (which "nm"))
                (setenv "SHELL" (which "sh"))
                (setenv "CCFLAGS"
                        (string-append
                         "-fno-strict-aliasing -Wno-unknown-pragmas "
                         "-Wno-missing-braces -Wno-unused-result "
                         "-Wno-return-type -Wno-int-to-pointer-cast "
                         "-Wno-parentheses -Wno-unused -Wno-cpp "
                         "-Wno-maybe-uninitialized"))
                (apply invoke "sh" "bin/package" "make"
                       (if parallel-build?
                           (list (string-append "-j"
                                                (number->string (parallel-job-count))))
                           '()))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "sh" "bin/package" "test"))))
            (replace 'install
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (gcc-lib (string-append (assoc-ref inputs "gcc-toolchain") "/lib"))
                       (fun-src (string-append out "/share/fun"))
                       (include-src (string-append out "/include"))
                       (lib-src (string-append out "/lib"))
                       (man3-src (string-append out "/share/man/man3"))
                       (fun-dst (assoc-ref outputs "fun"))
                       (include-dst (assoc-ref outputs "include"))
                       (lib-dst (string-append (assoc-ref outputs "lib") "/lib"))
                       (man3-dst (string-append include-dst "/share/man/man3"))
                       (rpath (string-append lib-dst ":" gcc-lib)))
                  (invoke "sh" "bin/package" "install" out "ksh")
                  (rename-file include-src include-dst)
                  (rename-file fun-src fun-dst)
                  (copy-recursively man3-src man3-dst)
                  (delete-file-recursively man3-src)
                  (mkdir-p lib-dst)
                  (copy-recursively lib-src lib-dst)
                  (delete-file-recursively lib-src)
                  (for-each (lambda (file)
                              (invoke "patchelf" "--set-rpath" rpath file))
                            (append (find-files lib-dst "\\.so")
                                    (find-files bin ".*")))))))))
      (outputs '("out" "fun" "include" "lib"))
      (inputs (list bash-minimal))
      (native-inputs (list patchelf gcc-toolchain))
      (home-page "https://github.com/ksh93/ksh")
      (synopsis "Modern maintained version of KornShell 93")
      (description
       "KornShell is an interactive UNIX command interpreter as well as a
POSIX compliant scripting language which is a superset of sh.
This is a maintained modern fork of the original AT&T KornShell (ksh93).")
      (license license:epl2.0))))

(define-public ksh93u+m
  (deprecated-package "ksh93u+m" ksh))

ksh
