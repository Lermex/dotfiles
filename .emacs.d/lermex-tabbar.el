(require 'tabbar)

(setq default-bg nil)
(setq default-fg nil)
(setq hilight-bg "#333333")
(setq hilight-fg nil)

(setq tabbar-background-color nil) ;; the color of the tabbar background

(custom-theme-set-faces 'user
`(tabbar-default ((nil (:background ,default-bg :foreground ,default-fg :underline nil))))
`(tabbar-highlight ((t nil)))
`(tabbar-selected ((t (:inherit tabbar-default :foreground ,hilight-fg :background ,hilight-bg))))
`(tabbar-unselected ((t (:inherit tabbar-default))))
`(tabbar-separator ((t (:inherit tabbar-default :height 0.8 ))))
)

(setq
              tabbar-scroll-left-help-function nil ;don't show help information
              tabbar-scroll-right-help-function nil
              tabbar-help-on-tab-function nil
              tabbar-home-help-function nil
              tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
              tabbar-scroll-left-button (quote (("") ""))
              tabbar-scroll-right-button (quote (("") "")))

(tabbar-mode t)
