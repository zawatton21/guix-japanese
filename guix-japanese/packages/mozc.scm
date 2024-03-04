(define-module (guix-japanese packages mozc)
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
  #:use-module  (guix-japanese packages google)
  #:use-module  (guix licenses)
  #:use-module  (ice-9 rdelim)  ; read-string 関数のため
  #:use-module  (ice-9 regex)   ; 正規表現機能のため
  #:use-module  (ice-9 format) ; format 関数のため
  )

(define-public mozc-debian-patches
  (package
   (name "mozc-debian-patches")
   (version "2.28.4715.102")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.de.debian.org/debian/pool/main/m/mozc/mozc_"
                                version
                                "+dfsg-2.2.debian.tar.xz"))
            (sha256
             (base32 "1jzfcy17cj0ay7gfjn1gbw9w2dfryx3zgpfdfdg2pfg4rqah1n5j"))))
   (build-system gnu-build-system)
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
   (home-page "http://ftp.de.debian.org/debian/pool/main/m/mozc/")
   (synopsis "Debian patches for mozc")
   (description "This package provides the Debian patches for mozc.")
   (license gpl2+)))

(define mozc-common
  (package
   (name "mozc-common")
   (version "2.28.4715.102")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://ftp.de.debian.org/debian/pool/main/m/mozc/mozc_"
                  version "+dfsg.orig.tar.xz"))
            (sha256
             (base32 "1b1s2zqi032qgsjz7a53pq29d0vqfvvq4bh5cxs084x0bllpakv6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-1) ;; List操作のため
                 (ice-9 rdelim) ;; read-string関数のため
                 (ice-9 regex)  ;; 正規表現機能のため
                 (ice-9 format)) ;; format関数のため
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'post-patch
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let* ((chdir "src")
                                         (patch-dir (string-append (assoc-ref inputs "mozc-debian-patches") "/debian/patches"))
                                         (patches '("0001-Update-uim-mozc-to-c979f127acaeb7b35d3344e8b1e40848e.patch"
                                                    "0002-Support-fcitx.patch"
                                                    "0003-Change-compiler-from-clang-to-gcc.patch"
                                                    "0004-Add-usage_dict.txt.patch"
                                                    "0005-Enable-verbose-build.patch"
                                                    "0006-Update-gyp-using-absl.patch"
                                                    "0007-common.gypi-Use-command-v-instead-of-which.patch"
                                                    ;;"0008-renderer-Convert-Gtk2-to-Gtk3.patch" ;; Wayland でなく X11 を使用している時はgtk2で良いのでコメントアウト
                                                    "0009-protobuf.gyp-Add-latomic-to-link_settings.patch")))
                                    (for-each (lambda (patch)
                                                (invoke "patch" "-p1" "-i" (string-append patch-dir "/" patch)))
                                              patches))))
                     ;;(delete 'configure)
                     (replace 'configure
                              (lambda* (#:key inputs outputs #:allow-other-keys)
                                ;; 依存関係のパスを定義
                                (define unzip-bin (string-append (assoc-ref %build-inputs "unzip") "/bin"))
                                (define bash-bin (string-append (assoc-ref %build-inputs "bash") "/bin"))
                                (define coreutils-bin (string-append (assoc-ref %build-inputs "coreutils") "/bin"))
                                (define findutils-bin (string-append (assoc-ref %build-inputs "findutils") "/bin"))
                                (define grep-bin (string-append (assoc-ref %build-inputs "grep") "/bin"))
                                (define sed-bin (string-append (assoc-ref %build-inputs "sed") "/bin"))
                                (define which-bin (string-append (assoc-ref %build-inputs "which") "/bin"))
                                (define python-bin (string-append (assoc-ref %build-inputs "python") "/bin"))
                                (define qtbase-bin (string-append (assoc-ref %build-inputs "qtbase") "/bin"))
                                (define qttools-bin (string-append (assoc-ref %build-inputs "qttools") "/bin"))
                                (define gettext-bin (string-append (assoc-ref %build-inputs "gettext") "/bin"))
                                (define gtk2-bin (string-append (assoc-ref %build-inputs "gtk+") "/bin"))
                                (define fcitx-bin (string-append (assoc-ref %build-inputs "fcitx") "/bin"))
                                (define fcitx5-bin (string-append (assoc-ref %build-inputs "fcitx5") "/bin"))
                                (define uim-bin (string-append (assoc-ref %build-inputs "uim") "/bin"))

                                (setenv "HOME" (getcwd)) ;; 現在の作業ディレクトリをホームディレクトリとして設定
                                (setenv "PYTHON_BIN_PATH" (string-append (assoc-ref %build-inputs "python") "/bin"))
                                ;; 環境変数pathにjdkのbinディレクトリと他のツールのパスを追加
                                (setenv "PATH" (string-join (list uim-bin fcitx5-bin fcitx-bin gtk2-bin gettext-bin qtbase-bin qttools-bin which-bin python-bin sed-bin grep-bin findutils-bin coreutils-bin unzip-bin bash-bin (getenv "PATH")) ":"))
                                (setenv "PKG_CONFIG_PATH"
                                        (string-join
                                         (list (string-append (assoc-ref inputs "qtbase") "/lib/pkgconfig")
                                               (string-append (assoc-ref inputs "qttools") "/lib/pkgconfig")
                                               (string-append (assoc-ref inputs "fcitx5") "/lib/pkgconfig")
                                               (getenv "PKG_CONFIG_PATH"))
                                         ":"))

                                (chdir "src")
                                (setenv "GYP_DEFINES" " use_libzinnia=1 use_libprotobuf=1 use_libabseil=1")
                                #t))
                     (delete 'check))))
   (native-inputs
    `(("mozc-debian-patches" ,mozc-debian-patches)
      ("ninja" ,ninja)
      ("python" ,python-wrapper)
      ("python-six" ,python-six)
      ("python-gyp" ,python-gyp)
      ("qtbase" ,qtbase-5)
      ("qttools" ,qttools-5)
      ("gettext" ,gnu-gettext)
      ("sed" ,sed)
      ("xdg-utils" ,xdg-utils)
      ("zip" ,zip)
      ("unzip" ,unzip)
      ("coreutils" ,coreutils)
      ("findutils" ,findutils)
      ("gtk+" ,gtk+-2) ;; Wayland でなく X11 を使用している時は Gtk2 でないと変換候補が表示されない
      ("grep" ,grep)
      ("sed" ,sed)
      ("which" ,which)
      ("bash", bash)
      ("fcitx", fcitx)
      ("fcitx5" ,fcitx5)
      ("uim", uim)))
   (inputs
    `(("which" ,which)
      ("ninja" ,ninja)
      ("python" ,python)
      ("pkg-config" ,pkg-config)
      ("protobuf" ,protobuf)
      ("gtk+" ,gtk+-2) ;; Wayland でなく X11 を使用している時は Gtk2 でないと変換候補が表示されない
      ("zinnia" ,zinnia)
      ("qtbase" ,qtbase)
      ("libxcb" ,libxcb)
      ("abseil" ,abseil)))
   (synopsis "japanese input method from google")
   (description "this package provides the mozc input method for japanese, developed by google.")
   (home-page "https://github.com/google/mozc")
   (license gpl3+)))

(define-public mozc-server
  (package
    (inherit mozc-common)
    (name "mozc-server")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc-common)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out (assoc-ref outputs "out"))
                      (define mozc-dir (string-append out "/lib/mozc"))
                      (define gyp-bin (string-append (assoc-ref %build-inputs "python-gyp") "/bin"))
                      (setenv "PATH" (string-join (list gyp-bin (getenv "PATH")) ":"))

                      ;; bazelビルドスクリプトの実行
                      (invoke "python3" "build_mozc.py" "gyp" (string-append "--gypdir=" gyp-bin) (string-append "--server_dir=" mozc-dir) "--target_platform=Linux" "--verbose")
                      (invoke "python3" "build_mozc.py" "build" "-c" "Release" "server/server.gyp:mozc_server" "renderer/renderer.gyp:mozc_renderer" "gui/gui.gyp:mozc_tool")
                      #t))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
               	             (mozc-dir (string-append out "/lib/mozc"))
                             (share-dir (string-append out "/share/ibus/component"))
                             (images-dir (string-append out "/share/ibus-mozc"))
                             (applications-dir (string-append out "/share/applications"))
                             (debian-patches-dir (assoc-ref inputs "mozc-debian-patches"))
                             (desktop-files-source-dir (string-append debian-patches-dir "/debian"))
                             (exec-path (string-append out "/lib/mozc/mozc_tool"))
                             (icon-path (string-append out "/share/ibus-mozc/product_icon.png")))

                        (mkdir-p mozc-dir)
                        (mkdir-p images-dir)

                        ;; 実行ファイルやリソースファイルのコピー
                        (copy-recursively "out_linux/Release/mozc_server" (string-append mozc-dir "/mozc_server"))
                        (copy-recursively "out_linux/Release/mozc_renderer" (string-append mozc-dir "/mozc_renderer"))
                        (copy-recursively "out_linux/Release/mozc_tool" (string-append mozc-dir "/mozc_tool"))
                                                                    
                        (copy-recursively "data/images/unix/ui-alpha_full.png" (string-append images-dir "/alpha_full.png"))
                        (copy-recursively "data/images/unix/ui-alpha_half.png" (string-append images-dir "/alpha_half.png"))
                        (copy-recursively "data/images/unix/ui-dictionary.png" (string-append images-dir "/dictionary.png"))
                        (copy-recursively "data/images/unix/ui-direct.png" (string-append images-dir "/direct.png"))
                        (copy-recursively "data/images/unix/ui-hiragana.png" (string-append images-dir "/hiragana.png"))
                        (copy-recursively "data/images/unix/ui-katakana_full.png" (string-append images-dir "/katakana_full.png"))
                        (copy-recursively "data/images/unix/ui-katakana_half.png" (string-append images-dir "/katakana_half.png"))
                        (copy-recursively "data/images/unix/ime_product_icon_opensource-32.png" (string-append images-dir "/product_icon.png"))
                        (copy-recursively "data/images/unix/ui-properties.png" (string-append images-dir "/properties.png"))
                        (copy-recursively "data/images/unix/ui-tool.png" (string-append images-dir "/tool.png"))        
                        
                        (mkdir-p applications-dir)
                                    
                        ;; setup-mozc.desktop の処理
                        (let ((source-file (string-append desktop-files-source-dir "/setup-mozc.desktop"))
                              (destination-file (string-append applications-dir "/setup-mozc.desktop")))
                          (copy-file source-file destination-file)
                          (substitute* destination-file
                                       (("Exec=/usr/lib/mozc/mozc_tool --mode=config_dialog" _)
                                        (string-append "Exec=" exec-path " --mode=config_dialog"))
                                       (("Icon=/usr/share/icons/mozc/product_icon_32bpp-128.png" _)
                                        (string-append "Icon=" icon-path))))
                        #t)))))))))

(define-public mozc-emacs-helper
  (package
    (inherit mozc-common)
    (name "mozc-emacs-helper")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc-common)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out (assoc-ref outputs "out"))
                      (let ((mozc-server-path (assoc-ref inputs "mozc-server")))
                        (unless mozc-server-path
                          (error "mozc-server input not found"))
                        (define mozc-server-dir (string-append mozc-server-path "/lib/mozc"))
                        (define gyp-bin (string-append (assoc-ref %build-inputs "python-gyp") "/bin"))
                        (setenv "PATH" (string-join (list gyp-bin (getenv "PATH")) ":"))

                        ;; bazelビルドスクリプトの実行
                        (invoke "python3" "build_mozc.py" "gyp" (string-append "--gypdir=" gyp-bin) (string-append "--server_dir=" mozc-server-dir) "--target_platform=Linux" "--verbose")
                        (invoke "python3" "build_mozc.py" "build" "-c" "Release" "unix/emacs/emacs.gyp:mozc_emacs_helper")
                        #t)))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin-dir (string-append out "/bin")))
                        ;; `out_linux/`ディレクトリから必要なファイルをインストールディレクトリにコピー
                        (mkdir-p bin-dir)
                        ;; 実行ファイルやリソースファイルのコピー
                        (copy-recursively "out_linux/Release/mozc_emacs_helper" (string-append bin-dir "/mozc_emacs_helper"))
                        #t)))))))
    (inputs
     `(("mozc-server" ,mozc-server)
       ,@(package-inputs mozc-common)))))
