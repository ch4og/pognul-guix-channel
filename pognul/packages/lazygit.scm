;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <mitanick@ya.ru>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pognul packages lazygit)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public lazygit
  (package
    (name "lazygit")
    (version "0.52.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jesseduffield/lazygit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nijsnx9nq8kdmjcx9g2fxbj6rmx2wqy1xr5aysgzlc1ysi53cdm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jesseduffield/lazygit"
      #:go go-1.24
      #:tests? #f
      #:install-source? #f))
    (propagated-inputs (list git-minimal))
    (home-page "https://github.com/jesseduffield/lazygit")
    (synopsis "Simple terminal UI for git commands")
    (description "Simple terminal UI for git commands")
    (license license:expat)))

lazygit
