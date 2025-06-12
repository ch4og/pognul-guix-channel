(define-module (pognul packages lazygit)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (guix licenses))

(define-public lazygit
  (package
    (name "lazygit")
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jesseduffield/lazygit/archive/refs/tags/v" version ".tar.gz"))
              (sha256 (base32 "1y363yn884d1mshzgdnwf6dgjw5b9f1si7zam6s6z8yc0m8h8srd"))
              (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/jesseduffield/lazygit"
	   #:go go-1.24
           #:tests? #f
           #:install-source? #f))
    (home-page "https://github.com/jesseduffield/lazygit")
    (synopsis "Simple terminal UI for git commands")
    (description "Simple terminal UI for git commands")
    (license expat)))
