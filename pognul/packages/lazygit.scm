;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pognul packages lazygit)
  #:use-module (guix packages)
  #:use-module (pognul build-system nix-go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public lazygit
  (package
    (name "lazygit")
    (version "0.53.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jesseduffield/lazygit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "188g2v159hvnjd0cz61mj9c45m17i8a1brsz1d6lah2i2z0irmxv"))))
    (build-system nix-go-build-system)
    (arguments
     `(#:vendor-hash "06shyxk6nh890gn8n0lzl6ri04yc53f2zm8szqvcf74j05s339hb"
       #:go ,go-1.24
       #:ldflags `("-X" ,(string-append "main.version=" ,version)
                   "-X" "'main.buildSource=ch4og/pognul Guix channel'")))
    (propagated-inputs (list git-minimal))
    (home-page "https://github.com/jesseduffield/lazygit")
    (synopsis "Simple terminal UI for git commands")
    (description "Simple terminal UI for git commands")
    (license license:expat)))

lazygit
