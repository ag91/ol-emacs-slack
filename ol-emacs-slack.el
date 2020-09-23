;;; ol-emacs-slack.el --- Support for links to emacs-slack chats in Org mode

;; Copyright (C) 2020 Andrea Giugliano

;; Author: Andrea Giugliano <andrea-dev@hotmail.com>
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See documentation on https://github.com/ag91/ol-emacs-slack/
(require 'ol)
(require 'dash)
(require 's)

(org-link-set-parameters "emacs-slack"
                         :follow #'ol/slack-follow-link
                         :export #'ol/slack-export
                         :store #'ol/slack-store-link)

(defun ol/slack-string-to-team (team)
  "Convert TEAM name to team object."
  (let ((slack-completing-read-function
         (lambda (prompt collection &optional predicate require-match
                         initial-input hist def inherit-input-method)
           (s-trim team))))
    (slack-team-select)))

(defun ol/room-name-equal (room channel-room)
  "Check ROOM is equal to CHANNEL-ROOM."
  (string=
   (s-downcase (s-trim room))
   (s-downcase
    (let ((trimmed (s-trim (s-chop-prefix " * " channel-room))))
      (if (> (length trimmed) (length room))
          (substring trimmed 0 (length room))
        trimmed)))))

(defun ol/slack-room-select (room rooms team)
  "Select ROOM from ROOMS and TEAM."
  (let* ((alist (slack-room-names
                 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs))))
         (selected (cdr (cl-assoc room alist :test 'ol/room-name-equal))))
    selected))

(defun ol/slack-select-channel (team-object room-with-prefix)
  "Return channel object from TEAM-OBJECT and ROOM-WITH-PREFIX string (as comes out from alert)."
  (let ((room (second (s-split " - " room-with-prefix))))
    (when (or
           (s-lowercase? room)
           (s-contains? "Thread in #" room))
      (ol/slack-room-select
       (s-trim (s-replace "#" "" (s-replace "Thread in #" "" room-with-prefix)))
       (slack-team-channels team-object)
       team-object))))

(defun ol/slack-select-group (team-object room-with-prefix)
  "Return group object from TEAM-OBJECT and ROOM-WITH-PREFIX string."
  (let ((room (second (s-split " - " room-with-prefix))))
    (when (and
           (s-lowercase? room)
           (s-contains? "--" room))
      (ol/slack-room-select
       (s-trim (s-replace "#" "" (s-replace "Thread in #" "" room-with-prefix)))
       (slack-team-groups team-object)
       team-object))))

(defun ol/slack-select-im (team-object room-with-prefix)
  "Return im object from TEAM-OBJECT and ROOM-WITH-PREFIX string."
  (let ((room (second (s-split " - " room-with-prefix))))
    (ol/slack-room-select
     (s-trim (s-replace "#" "" (s-replace "Thread in #" "" room-with-prefix)))
     (slack-team-ims team-object)
     team-object)))

(defun ol/slack-string-to-room (team-object room)
  "Convert TEAM-OBJECT and ROOM name to room object."
  (or
   (ol/slack-select-channel team-object room)
   (ol/slack-select-group team-object room)
   (ol/slack-select-im team-object room)))

(defun ol/slack-follow-link (link)
  "Follow LINK with format `   team - channel'."
  (let* ((team (--> link
                    (s-split "-" it)
                    first
                    s-trim))
         (team-object (ol/slack-string-to-team team)))
    (slack-room-display (ol/slack-string-to-room team-object link) team-object)))

(defun ol/slack-store-link ()
  "Store a link to a man page."
  (when (eq major-mode 'slack-message-buffer-mode)
    (let* ((buf slack-current-buffer)
           (team (slack-buffer-team buf))
           (team-name (oref team name))
           (room (slack-buffer-room buf))
           (room-name (slack-room-name room team))
           (link (funcall
                  slack-message-notification-title-format-function
                  team-name
                  room-name
                  (cl-typep buf 'slack-thread-message-buffer)))
           (description ))
      (org-link-store-props
       :type "emacs-slack"
       :link (concat "emacs-slack:" link)
       :description (concat "Slack message in #" room-name)
       ))))

(defun ol/slack-export (link description format)
  "Export a emacs-slack link from Org files."
  (or description link))

(provide 'ol-emacs-slack)
;;; ol-emacs-slack.el ends here
