;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cgoogle-auto-index-on-load t)
 '(cgoogle-index-stdlib-on-load t)
 '(cgoogle-stdlib-essential-headers
   '("stdio.h" "stdlib.h" "string.h" "math.h" "pthread.h" "unistd.h"
     "dirent.h"))
 '(company-format-margin-function nil)
 '(custom-safe-themes
   '("d543f0f80da544f74d9902759ab01143900a9c1602dd3715598c08b1fd4a9584"
     "74e7de508d5813d810b7c77980c554b0ac852e6032ea9042bccf1b0e90e2a9ef"
     "c68bad4015e9997097e3a4d0ad2be19af459c9dde4386586546ce2f70560599c"
     "b5732e233b019c9a616a623d2609344a7d2d1358ace87d0e06a9fde410db002c"
     "633de145e8d8e55bf81c605d65348d5e470564af525bade3b13d1e63f1df4668"
     "d35094fa5f896e62caed89acd56346d59755174c619d43aad778d1ab0715ce9d"
     "34a59305dc48cfb706403a00f4411eb7249ea2833694401520d88de00704bcec"
     "06c4fbb01a78b5cef26b3a9f75af8d17e02c75c30b360b809cee593171b1592c"
     "63508f469445bb2cf410f1d9c113b920daea35a977d167cfa3158dff7f729a14"
     "7597c5218bfd22e4543a04d6741e445c58b9b069554d1d7468ea8f3ef7a05e68"
     "7b1f1c52ed0bacfed1bd9bbeeaefd5fb05f57bc9d542f18a12ee5cb797f1bf08"
     "b3a1eacb9fe9b5baff805104d0b6b2a1b53bc5d6db7896e67ff59d51eff81ba4"
     "33f06c4736b5ba9dcd1adc7ebe55733f262d9e6dea2c7753596c2eea6053a17b"
     "5b7dca9c78f0c8d1150c9929d8b3103536fe42ddbc0bab065f4af4bea99432ff"
     "44f3fabd069f77bf9db57f9328203f654c06d87cdcd38ae3f0eeaa0b8c7bf56c"
     "4892f868b339175325f7d6bfcadcbbec5e881744b777847092c65a8b36b94de9"
     "789c42f7ca8e8f7e8d692a3cc8c3fa82d40d11a9d392f1f2ac4d0592961d2766"
     "693c2c54c85ca9daf1c9145a6a295ffd750ccbb61d70dd3e71572fef28d59307"
     "8ccb898c5308e5158512a12d7be2982d718a12a951c7ad1de6b60ea4bd06e199"
     "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     "caeb6faf66081ab38498a75fadad09abeea4ff75d6093ae57d94b80305f0b7ec"
     "347e8979e3ff98b4707bab5d793c2b22d4006643198976eaced7f98af60354fc"
     "f7ced2c7d9d5de211c3683ad46543fa636c05c26366c9434498182bff0dd91e0"
     "5c2aedf48b1ad6316381798af5b320899d6ec760ab0ced6bf81cce6de0a8dc22"
     "dc22aeffeed97234785b1328663045c6d15306e7abfc104b347c1687ffc680ea"
     "fb76e4b5b538e40bc85a3e0ab23912ed7c5ace224111b7d719ddd4e32ba55347"
     "6aecca9942d29a61204fe3070850cc3832df5d956f968fcf41d46c2b3746902c"
     "c41ee1b2d010c42df06371f7f5e9d348e4ae4fc8fcd55ce2ceb8ac30f783e5f3"
     "ef8dfed495cf8e262e1eb0c9574171f19ff0bef7e97a2dc77d5fd22b5416088c"
     "8a9fef3a6778ad6e0cae765d1ebd4805c58868712ffda1da02b5d70ac798edce"
     "5cf12a54172956d44e1e44495cea9705468489e8b569a1d1ad301c2bca8a5503"
     default))
 '(dired-listing-switches "-alh")
 '(display-line-numbers-type 'visual)
 '(global-display-line-numbers-mode t)
 '(grep-command "grep -rn ")
 '(grep-find-command
   '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +"
     . 54))
 '(grep-find-template
   "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
 '(grep-find-use-xargs 'exec-plus)
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH --null -e <R> <F>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator t)
 '(line-number-mode nil)
 '(package-selected-packages nil)
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
