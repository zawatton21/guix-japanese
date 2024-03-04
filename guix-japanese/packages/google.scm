(define-module (guix-japanese packages google)
  #:use-module  (guix packages)
  #:use-module  (guix download)
  #:use-module  (guix git-download)
  #:use-module  (guix utils)
  #:use-module  (guix build-system trivial)
  #:use-module  (guix build-system gnu)
  #:use-module  (gnu packages cmake)         
  #:use-module  (guix build-system cmake)
  #:use-module  (gnu packages)
  #:use-module  (gnu packages ninja)
  #:use-module  (gnu packages fcitx)
  #:use-module  (gnu packages fcitx5)
  #:use-module  (gnu packages gettext)
  #:use-module  (gnu packages ocr)
  #:use-module  (gnu packages autotools)
  #:use-module  (gnu packages base)
  #:use-module  (gnu packages compression)
  #:use-module  (gnu packages language)
  #:use-module  (gnu packages glib)
  #:use-module  (gnu packages gnome)
  #:use-module  (gnu packages bash)
  #:use-module  (gnu packages gtk)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages protobuf)
  #:use-module  (gnu packages java)
  #:use-module  (gnu packages python)
  #:use-module  (gnu packages python-xyz)
  #:use-module  (gnu packages qt)
  #:use-module  (gnu packages xorg)
  #:use-module  (gnu packages freedesktop)
  #:use-module  (gnu packages dictionaries)
  #:use-module  (gnu packages ibus)
  #:use-module  (guix licenses)
  #:use-module  (ice-9 rdelim)  ; read-string 関数のため
  #:use-module  (ice-9 regex)   ; 正規表現機能のため
  #:use-module  (ice-9 format) ; format 関数のため
  )

(define-public abseil-debian-patches
  (package
    (name "abseil-debian-patches")
    (version "20220623.1-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.de.debian.org/debian/pool/main/a/abseil/abseil_"
                                  version
                                  ".debian.tar.xz"))
              (sha256
               (base32 "01n0anicg3y2dqq3xk0gv3fsfi6jdqhrmwdl4v04y4ah8z0c6cm2"))))
    (build-system gnu-build-system) ; build-systemをgnu-build-systemに変更
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (source (assoc-ref %build-inputs "source")))
               (mkdir-p out)
               (invoke "tar" "xf" source "-C" out)
               #t))))))
    (home-page "http://ftp.de.debian.org/debian/pool/main/a/abseil/")
    (synopsis "Debian patches for Abseil")
    (description "This package provides the Debian patches for Abseil.")
    (license gpl2+)))

(define-public abseil
  (package
   (name "abseil")
   (version "20220623.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.de.debian.org/debian/pool/main/a/abseil/abseil_"
                                version
                                ".orig.tar.gz"))                 
            (sha256
             (base32 "1znmvim03xq8w33pvaci400wv8y2wfpnahy39fkxl3m3yfbjizmb"))))
   (build-system cmake-build-system)
   (arguments
    `(#:modules ((guix build cmake-build-system)
                 (guix build utils)
                 (srfi srfi-1)
                 (ice-9 rdelim)
                 (ice-9 regex)
                 (ice-9 format))
      #:configure-flags
      (list (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "out"))
            "-DBUILD_SHARED_LIBS=ON")
      #:phases
      (modify-phases %standard-phases
                     (add-before 'configure 'apply-debian-patches
                                 (lambda* (#:key inputs outputs #:allow-other-keys)
                                   (let* ((patch-dir (string-append (assoc-ref inputs "abseil-debian-patches") "/debian/patches"))
                                          (patches '("configure.diff"
                                                     "cordz-info-statistics-test.diff"
                                                     "cpu-features.diff"
                                                     "empty-flags-library.diff"
                                                     "latomic.diff"
                                                     "leaky-pkgconfig-cflags.diff")))
                                     (for-each (lambda (patch)
                                                 (invoke "patch" "-p1" "-i" (string-append patch-dir "/" patch)))
                                               patches)))))))
   (native-inputs
    `(("cmake" ,cmake)
      ("abseil-debian-patches" ,abseil-debian-patches)))
   (home-page "https://github.com/abseil/abseil-cpp")
   (synopsis "Abseil C++ Common Libraries")
   (description "Abseil is an open-source collection of C++ library code
developed at Google. The code is designed to augment the C++
standard library.")
   (license gpl2+)))
