;;; Copyright © 2025 Nikita Mitasov <mitanick@ya.ru>
;;;
;;; This file is NOT part of GNU Guix.

(define-module (pognul packages wakatime)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages skarnet)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:))

(define go-github-com-go-viper-encoding-ini-for-wakatime
  (package
   (name "go-github-com-go-viper-encoding-ini")
   (version "0.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/go-viper/encoding")
           (commit (string-append "ini/v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0q4kjnd67vipfjlscy7n5an9klfs2c8ijqk8h27p7bhbbz34gz0n"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/go-viper/encoding"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/go-viper/encoding")
   (synopsis "Viper encoding libraries")
   (description
    "This repository contains the encoding libraries (with external dependencies) used by Viper.")
   (license license:expat)))

(define go-github-com-matishsiao-goinfo-for-wakatime
  (let ((commit "66a9250504d696bad4ee09ac51b6ecb93362b451"))
    (package
     (name "go-github-com-matishsiao-goinfo")
     (version (git-version "0.0.0" "0" commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matishsiao/goInfo")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jqdrkq1ky3183kdbbcrvkjv8azx5xnrjhl8q0981l696dmykhvx"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "github.com/matishsiao/goInfo"
       #:install-source? #t
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases (delete 'build))))
     (home-page "https://github.com/matishsiao/goInfo")
     (synopsis "Get OS information in Go")
     (description "GoInfo is get os platform information coding by Golang.
It can help you to know os information.")
     (license license:expat))))

(define go-github-com-slongfield-pyfmt-for-wakatime
  (let ((commit "ea85ff4c361f54bcfc91ede46f88de49eb5f92e7"))
    (package
     (name "go-github-com-slongfield-pyfmt")
     (version (git-version "0.0.0" "0" commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slongfield/pyfmt")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10pcy0sm7c2sjbvhfycfdmlbbzcfqds2dipd9nlj1vp14ymwaykd"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "github.com/slongfield/pyfmt"
       #:install-source? #t
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases (delete 'build))))
     (home-page "https://github.com/slongfield/pyfmt")
     (synopsis "Golang implementation of PEP3101")
     (description
      "pyfmt implements Python's advanced string formatting in Golang.")
     (license license:bsd-3))))

(define go-github-com-gandarez-go-olson-timezone-for-wakatime
  (package
   (name "go-github-com-gandarez-go-olson-timezone")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/gandarez/go-olson-timezone")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13ny8gyckzp0wspbljq4jzm0zsvl8y1j7zjp8qf689fw079q20r1"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/gandarez/go-olson-timezone"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/gandarez/go-olson-timezone")
   (synopsis "Go library to figure out your local timezone")
   (description
    "A Golang library that tries to figure out your local timezone.")
   (license license:bsd-3)))

(define go-github-com-gandarez-go-realpath-for-wakatime
  (package
   (name "go-github-com-gandarez-go-realpath")
   (version "1.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/gandarez/go-realpath")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "18fm83z07dfn7afs6w4bcy2mrm9yg85qhicz08394q1rb89q4jlr"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/gandarez/go-realpath"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/gandarez/go-realpath")
   (synopsis "Realpath for Go")
   (description
    "This finds the true path of a file or directory, resolving any symbolic links.")
   (license license:bsd-3)))

(define go-github-com-juju-errors-for-wakatime
  (package
   (name "go-github-com-juju-errors")
   (version "1.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/juju/errors")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1j8c1kjg6iz6xdisfma024w4sjhm4vmsimyb5rrjkqpqsz079rpn"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/juju/errors"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/juju/errors")
   (synopsis "Common juju errors and functions to annotate errors")
   (description
    "The juju/errors provides an easy way to annotate errors without losing the original error context.")
   (license license:lgpl3)))

(define go-github-com-juju-mutex-for-wakatime
  (package
   (name "go-github-com-juju-mutex")
   (version "2.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/juju/mutex")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "013zcwpvc6lv7p0a1g6cf7wc92hhrpbc7ai0g4y8nwbvs86ypxfx"))))
   (native-inputs (list go-github-com-juju-errors-for-wakatime))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/juju/mutex"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/juju/mutex")
   (synopsis
    "Provides a named machine level mutex shareable between processes")
   (description
    "package mutex provides a named machine level mutex shareable between processes.")
   (license license:lgpl3)))

(define go-github-com-sagikazarmark-locafero-for-wakatime
  (package
   (name "go-github-com-sagikazarmark-locafero")
   (version "0.9.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sagikazarmark/locafero")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0xxxlw0z099mgg5phxvfc6i4gwjrs62cq7qa76xv645hibddnpn7"))))
   (build-system go-build-system)
   (propagated-inputs (list go-github-com-sourcegraph-conc-for-wakatime))
   (arguments
    (list
     #:import-path "github.com/sagikazarmark/locafero"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/sagikazarmark/locafero")
   (synopsis "Finder library for Afero")
   (description "Finder library for Afero ported from go-finder.")
   (license license:expat)))

(define go-github-com-sourcegraph-conc-for-wakatime
  (package
   (name "go-github-com-sourcegraph-conc")
   (version "0.3.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sourcegraph/conc")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1anqhnpiak7fd6xxrjanwgrfz3c8ypksmx3zgx5f000bsfrlr1wq"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/sourcegraph/conc"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   (home-page "https://github.com/sourcegraph/conc")
   (synopsis "Better structured concurrency for go")
   (description
    "conc is your toolbelt for structured concurrency in go, making common tasks easier and safer.")
   (license license:expat)))

(define go-github-com-spf13-viper-for-wakatime
  (package
   (inherit go-github-com-spf13-viper)
   (name "go-github-com-spf13-viper")
   (version "1.20.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/spf13/viper")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05q7iv7qq0sg4l3wzqp1vi4g1jv5plyshgfxwpafhip1244bm3vl"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/spf13/viper"
     #:install-source? #t
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases (delete 'build))))
   ))

(define-public wakatime-cli
  (package
   (name "wakatime-cli")
   (version "1.115.5")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/wakatime/wakatime-cli")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05cy7rmwvmwbpb2i3vf9afd3m1ff4sf5n87v9z6gvzgkmm8cjdna"))))
   (build-system go-build-system)
   (native-inputs (list go-github-com-gandarez-go-olson-timezone-for-wakatime
			go-github-com-gandarez-go-realpath-for-wakatime
			go-github-com-go-viper-encoding-ini-for-wakatime
			go-github-com-juju-errors-for-wakatime
			go-github-com-juju-mutex-for-wakatime
			go-github-com-matishsiao-goinfo-for-wakatime
			go-github-com-sagikazarmark-locafero-for-wakatime
			go-github-com-slongfield-pyfmt-for-wakatime
			go-github-com-spf13-viper-for-wakatime

			go-github-com-alecthomas-chroma-v2
			go-github-com-azure-go-ntlmssp
			go-github-com-danwakefield-fnmatch
			go-github-com-dlclark-regexp2
			go-github-com-go-viper-mapstructure-v2
			go-github-com-kevinburke-ssh-config
			go-github-com-mitchellh-go-homedir
			go-github-com-pelletier-go-toml-v2
			go-github-com-spf13-cobra
			go-github-com-yookoala-realpath
			go-go-etcd-io-bbolt
			go-go-uber-org-zap
			go-golang-org-x-net
			go-gopkg-in-natefinch-lumberjack-v2

			bats
			execline
			perl
			python))
   (arguments
    (list
     #:import-path "github.com/wakatime/wakatime-cli"
     #:go go-1.24
     #:tests? #f
     #:install-source? #f
     #:embed-files
     #~(list ".*\\.xml")))

   (home-page "https://github.com/wakatime/wakatime-cli")
   (synopsis "CLI for WakaTime")
   (description
    "Command line interface to WakaTime used by all WakaTime text editor plugins.")
   (license license:bsd-3)))

wakatime-cli
