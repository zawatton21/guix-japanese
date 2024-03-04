(define-module (guix-japanese packages fcitx5)
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
  #:use-module  (guix-japanese packages mozc)
  #:use-module  (guix licenses)
  #:use-module  (ice-9 rdelim)  ; read-string 関数のため
  #:use-module  (ice-9 regex)   ; 正規表現機能のため
  #:use-module  (ice-9 format) ; format 関数のため
  )

(define-public fcitx5-mozc
  (package
    (inherit mozc-server)
    (name "fcitx5-mozc")
    (arguments
     (substitute-keyword-arguments (package-arguments mozc-server)
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
                      (invoke "python3" "build_mozc.py" "build" "-c" "Release" "unix/fcitx5/fcitx5.gyp:fcitx5-mozc")
                      #t)))
           (add-after 'build 'compile-po-files
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((po-dir "unix/fcitx5/po/"))

                          (for-each (lambda (lang)
                                      (let* ((po-file (string-append po-dir lang ".po"))
                                             (mo-file (string-append po-dir lang ".mo")))
                                        (mkdir-p (dirname mo-file))
                                        (invoke "msgfmt" po-file "-o" mo-file)))
                                    '("ca" "da" "de" "he" "ja" "ko" "ru" "zh_CN" "zh_TW"))
                          #t)))
           (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib-dir (string-append out "/lib/fcitx5"))
                             (share-dir (string-append out "/share"))
                             (in-dir "unix/fcitx5/")
                             (locale-dir (string-append share-dir "/locale"))
                             (metainfo-dir (string-append share-dir "/metainfo/"))
                             (addon-dir (string-append share-dir "/fcitx5/addon"))
                             (inputmethod-dir (string-append share-dir "/fcitx5/inputmethod")))
                        ;; モジュールファイルのインストール
                        (mkdir-p lib-dir)
                        (copy-file "out_linux/Release/fcitx5-mozc.so" (string-append lib-dir "/fcitx5-mozc.so"))
                        
                        ;; 設定ファイルのインストール
                        (mkdir-p addon-dir)
                        (copy-file "unix/fcitx5/mozc-addon.conf" (string-append addon-dir "/mozc.conf"))
                        
                        (mkdir-p inputmethod-dir)
                        (copy-file "unix/fcitx5/mozc.conf" (string-append inputmethod-dir "/mozc.conf"))
                        
                        ;; 翻訳ファイルのインストール
                        (let ((po-files '("ca" "da" "de" "he" "ja" "ko" "ru" "zh_CN" "zh_TW")))
                          (for-each (lambda (lang)
                                      (let ((mo-file-path (string-append "unix/fcitx5/po/" lang ".mo"))
                                            (target-dir (string-append locale-dir "/" lang "/LC_MESSAGES")))
                                        (mkdir-p target-dir)
                                        (copy-file mo-file-path (string-append target-dir "/fcitx5-mozc.mo"))))
                                    po-files))

                        ;; .in ファイルから .xml ファイルを生成
                        (let ((in-file (string-append in-dir "org.fcitx.Fcitx5.Addon.Mozc.metainfo.xml.in"))
                              (out-file (string-append metainfo-dir "org.fcitx.Fcitx5.Addon.Mozc.metainfo.xml")))
                          (mkdir-p metainfo-dir)
                          (system* "sed"
                                   "-e" "s|@VERSION@|2.28.4715.102|g"
                                   "-i" "" ; インプレース編集を行い、元のファイルを変更する
                                   in-file)
                          (copy-file in-file out-file))
                        #t)))
           (add-after 'install 'install-icon-files
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (lib-dir (string-append out "/lib/fcitx5"))
                               (share-dir (string-append out "/share"))
                               (icons-dir (string-append share-dir "/icons/hicolor"))
                               (mozc-icons-src-dir "data/images/unix")
                               (icon-sizes '("32x32" "48x48" "128x128")))
                          
                          ;; アイコンファイルのインストール
                          ;; アイコンファイルをソースディレクトリから適切なアイコンディレクトリにコピー
                          (for-each (lambda (size)
                                      (let* ((size-dir (string-append icons-dir "/" size "/apps")))
                                        (mkdir-p size-dir)
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ime_product_icon_opensource-32.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ime_product_icon_opensource-32.png")
                                                   (string-append size-dir "/fcitx-mozc.png"))
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ui-tool.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-tool.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-tool.png")
                                                   (string-append size-dir "/fcitx-mozc-tool.png"))
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ui-properties.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-tool.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-properties.png")
                                                   (string-append size-dir "/fcitx-mozc-tool.png"))
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ui-dictionary.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-dictionary.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-dictionary.png")
                                                   (string-append size-dir "/fcitx-mozc-dictionary.png"))
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ui-direct.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-direct.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-direct.png")
                                                   (string-append size-dir "/fcitx-mozc-direct.png"))
                                        
                                        (copy-file (string-append mozc-icons-src-dir "/ui-hiragana.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-hiragana.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-hiragana.png")
                                                   (string-append size-dir "/fcitx-mozc-hiragana.png"))

                                        (copy-file (string-append mozc-icons-src-dir "/ui-katakana_half.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-katakana_half.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-katakana_half.png")
                                                   (string-append size-dir "/fcitx-mozc-katakana_half.png"))

                                        (copy-file (string-append mozc-icons-src-dir "/ui-katakana_full.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-katakana_full.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-katakana_full.png")
                                                   (string-append size-dir "/fcitx-mozc-katakana_full.png"))

                                        (copy-file (string-append mozc-icons-src-dir "/ui-alpha_half.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-alpha_half.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-alpha_half.png")
                                                   (string-append size-dir "/fcitx-mozc-alpha_half.png"))

                                        (copy-file (string-append mozc-icons-src-dir "/ui-alpha_full.png")
                                                   (string-append size-dir "/org.fcitx.Fcitx5.fcitx-mozc-alpha_full.png"))
                                        (copy-file (string-append mozc-icons-src-dir "/ui-alpha_full.png")
                                                   (string-append size-dir "/fcitx-mozc-alpha_full.png"))
                                        ))
                                    icon-sizes)
                          #t)))
           ))))
    (inputs
     `(("mozc-server" ,mozc-server)
       ("fcitx5" ,fcitx5)
       ,@(package-inputs mozc-server)))
))

(define-public fcitx5-skk
  (package
   (name "fcitx5-skk")
   (version "5.1.2")
  (source (origin
           (method git-fetch)
  	   (uri (git-reference
  		 (url "https://github.com/fcitx/fcitx5-skk.git")
		 (commit "5.1.2")))
           (sha256
            (base32
	     "19s8m6dsyd90jlwd4vqgwvs7rfsjcvb83wh0k3bxx7sxqz7wyk11"))))
  (build-system cmake-build-system)
  (propagated-inputs
   (list libskk))
  (native-inputs
   `(("libgee" ,libgee)
     ("libskk" ,libskk)
     ("fcitx5" ,fcitx5)
     ("fcitx5-qt" ,fcitx5-qt)
     ("qtbase" ,qtbase)
     ("automake" ,automake)
     ("autoconf" ,autoconf)
     ("intltool" ,intltool)
     ("which" ,which)
     ("gnome-common" ,gnome-common)
     ("gtk+" ,gtk+)
     ("libtool" ,libtool)
     ("pkg-config" ,pkg-config)
     ("vala" ,vala)))
  (inputs
   `(("gtk+" ,gtk+)
     ("libgee" ,libgee)
     ("libskk" ,libskk)
     ("fcitx5" ,fcitx5)
     ("skktools" ,skktools)
     ("skk-jisyo" ,skk-jisyo)))
  (synopsis "a Japanese SKK input engine for Fcitx5")
  (description "fcitx5-skk is an input method engine for Fcitx5, which uses libskk as its backend..")
  (home-page "https://github.com/fcitx/fcitx5-skk")
  (license gpl2+)))
