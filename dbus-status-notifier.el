;;; dbus-status-notifier.el --- D-BUS Status Notifier Items  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierItem/

;; http://www.notmart.org/misc/statusnotifieritem/scenario.html

;; [2019-01-10 Thu 14:13] Well, I'm about done trying to get this to work.  I've followed the
;; FDO "spec" (which apparently isn't really a spec, just a convention that KDE uses and a bunch
;; of apps and other DEs imitate) and the apparently outdated example scenario that preceded the
;; FDO "spec", and I get no errors, but I get no icon.  I see the StatusNotifierItem services
;; registered in D-Bus, but no icons.  Just nothing.  I've spent nearly an hour digging through
;; Google and GitHub code in repos that create StatusNotifierItems, but I can't find anything
;; I'm doing wrong.  It just feels like a huge, half-baked, half-documented mess.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)

;;;; Variables


;;;; Customization

;;;; Classes

(defclass dbus-status-notifier-item ()
  ;; NOTE: The docstrings are copied directly from the spec.  There does
  ;; not seem to be any license mentioned on <freedesktop.org/wiki>.
  (
   ;; Internal data
   (service
    :type string
    :documentation "D-Bus name for service this notifier item provides.")
   (interface
    :type string
    :documentation "D-Bus interface.
I don't understand this at all.")
   (watcher
    :type string
    :documentation "D-Bus StatusNotifierWatcher service watching this item.
Likely, if not always, also the interface.")

   ;; Properties
   (category
    :initarg :category
    :initform "ApplicationStatus"
    :type string
    :documentation "Describes the category of this item.

The allowed values for the Category property are:

ApplicationStatus: The item describes the status of a generic
application, for instance the current state of a media player. In
the case where the category of the item can not be known, such as
when the item is being proxied from another incompatible or
emulated system, ApplicationStatus can be used a sensible default
fallback.

Communications: The item describes the status of communication
oriented applications, like an instant messenger or an email
client.

SystemServices: The item describes services of the system not
seen as a stand alone application by the user, such as an
indicator for the activity of a disk indexing service.

Hardware: The item describes the state and control of a
particular hardware, such as an indicator of the battery charge
or sound card volume control.")
   (id
    :initarg :id
    :type string
    :documentation "It's a name that should be unique for this application and consistent between sessions, such as the application name itself.")
   (title
    :initarg :title
    :type string
    :documentation "It's a name that describes the application, it can be more descriptive than Id.")
   (status
    :initarg :status
    ;; TODO: Only three values are allowed: Passive, Active, and NeedsAttention.  "Active" seems like
    ;; the best default, since the spec says that "Passive" will likely be hidden by default.
    :initform "Active"
    :type string
    :documentation "Describes the status of this item or of the associated application.

The allowed values for the Status property are:

Passive: The item doesn't convey important information to the
user, it can be considered an \"idle\" status and is likely that
visualizations will chose to hide it.

Active: The item is active, is more important that the item will
be shown in some way to the user.

NeedsAttention: The item carries really important information for
the user, such as battery charge running out and is wants to
incentive the direct user intervention. Visualizations should
emphasize in some way the items with NeedsAttention status.")
   (window-id
    ;; 0 if not interested.
    :initarg :window-id
    :initform 0
    :type integer
    :documentation "Window system-dependent window identifier.
It's the windowing-system dependent identifier for a window, the
application can chose one of its windows to be available through
this property or just set 0 if it's not interested.")
   (icon-name
    :initarg :icon-name
    :type string
    :documentation "Icon name.
The StatusNotifierItem can carry an icon that can be used by the
visualization to identify the item.

An icon can either be identified by its Freedesktop-compliant
icon name, carried by this property of by the icon data itself,
carried by the property IconPixmap. Visualizations are encouraged
to prefer icon names over icon pixmaps if both are
available (FIXME: still not very defined: could e the pixmap used
as fallback if an icon name is not found?)")
   (icon-pixmap
    :initarg :icon-pixmap
    :type array
    :documentation "Icon pixmap.
Carries an ARGB32 binary representation of the icon, the format
of icon data used in this specification is described in Section
Icons")
   (overlay-icon-name
    :initarg :overlay-icon-name
    :type string
    :documentation "The Freedesktop-compliant name of an icon.
This can be used by the visualization to indicate extra state
information, for instance as an overlay for the main icon.")
   (overlay-icon-pixmap
    :initarg :overlay-icon-pixmap
    :type array
    :documentation "Overlay icon pixmap.
ARGB32 binary representation of the overlay icon described in the
previous paragraph.")
   (attention-icon-name
    :initarg :attention-icon-name
    :type string
    :documentation "Attention icon name.
The Freedesktop-compliant name of an icon. this can be used by
the visualization to indicate that the item is in
RequestingAttention state.")
   (attention-icon-pixmap
    :initarg :attention-icon-pixmap
    :type array
    :documentation "Attention icon pixmap.
ARGB32 binary representation of the requesting attention icon
describe in the previous paragraph.")
   (attention-movie-name
    :initarg :attention-movie-name
    :type string
    :documentation "Attention animation name or path.
An item can also specify an animation associated to the
RequestingAttention state. This should be either a
Freedesktop-compliant icon name or a full path. The visualization
can chose between the movie or AttentionIconPixmap (or using
neither of those) at its discretion.")
   (tool-tip
    :initarg :tool-tip
    :type list
    :documentation "Tooltip data structure.
Data structure that describes extra information associated to
this item, that can be visualized for instance by a tooltip (or
by any other mean the visualization consider
appropriate. Components are:

STRING: Freedesktop-compliant name for an icon.

ARRAY(INT, INT, ARRAY BYTE): icon data

STRING: title for this tooltip

STRING: descriptive text for this tooltip. It can contain also a
subset of the HTML markup language, for a list of allowed tags
see Section Markup.")
   (item-menu-p
    :initarg :item-menu-p
    :type boolean
    :documentation "Whether the item only supports a context menu.
The item only support the context menu, the visualization should
prefer showing the menu or sending ContextMenu() instead of
Activate()")
   (menu
    :initarg :menu
    :documentation "Path to D-Bus menu object.
OBJECT PATH: DBus path to an object which should implement the
com.canonical.dbusmenu interface")

   ;; Methods
   (context-menu
    :initarg :context-menu
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.ContextMenu (INT x, INT y);

Asks the status notifier item to show a context menu, this is
typically a consequence of user input, such as mouse right click
over the graphical representation of the item.

the x and y parameters are in screen coordinates and is to be
considered an hint to the item about where to show the context
menu.")
   (activate
    :initarg :activate
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.Activate (INT x, INT y);

Asks the status notifier item for activation, this is typically a
consequence of user input, such as mouse left click over the
graphical representation of the item. The application will
perform any task is considered appropriate as an activation
request.

the x and y parameters are in screen coordinates and is to be
considered an hint to the item where to show eventual windows (if
any).")
   (secondary-activate
    :initarg :secondary-activate
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.SecondaryActivate (INT x, INT y);

Is to be considered a secondary and less important form of
activation compared to Activate. This is typically a consequence
of user input, such as mouse middle click over the graphical
representation of the item. The application will perform any task
is considered appropriate as an activation request.

the x and y parameters are in screen coordinates and is to be
considered an hint to the item where to show eventual windows (if
any).")
   (scroll
    :initarg :scroll
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.Scroll (INT delta, STRING orientation);

The user asked for a scroll action. This is caused from input
such as mouse wheel over the graphical representation of the
item.

The delta parameter represent the amount of scroll, the
orientation parameter represent the horizontal or vertical
orientation of the scroll request and its legal values are
horizontal and vertical.")

   ;; Signals
   (new-title
    :initarg :new-title
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewTitle ();

The item has a new title: the graphical representation should
read it again immediately.")
   (new-icon
    :initarg :new-icon
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewIcon ();

The item has a new icon: the graphical representation should read
it again immediately.")
   (new-attention-icon
    :initarg :new-attention-icon
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewAttentionIcon ();

The item has a new attention icon: the graphical representation
should read it again immediately.")
   (new-overlay-icon
    :initarg :new-overlay-icon
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewOverlayIcon ();

The item has a new overlay icon: the graphical representation
should read it again immediately.")
   (new-tool-tip
    :initarg :new-tool-tip
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewToolTip ();

The item has a new tooltip: the graphical representation should
read it again immediately.")
   (new-status
    :initarg :new-status
    :type function
    :initform #'ignore
    :documentation "VOID org.freedesktop.StatusNotifierItem.NewStatus (STRING status);

The item has a new status, that is passed as an argument of the
signal."))
  :allow-nil-initform t)

;;;; Commands


;;;; Functions

(defun dbus-status-notifier-item-register (bus item)
  "FIXME"
  ;; Each application can register an arbitrary number of Status Notifier Items by registering
  ;; on the session bus the service org.freedesktop.StatusNotifierItem-PID-ID, where PID is
  ;; the process id of the application and ID is an arbitrary numeric unique identifier
  ;; between different instances registered by the same application.
  ;;
  ;; TODO: As soon as a new instance of a StatusNotifierItem is created, the application must register
  ;; the unique instance name to the StatusNotifierWatcher as described in the Section called
  ;; StatusNotifierWatcher
  ;;
  ;; TODO: Each instance of StatusNotifierItem must provide an object called StatusNotifierItem with
  ;; the following properties, methods and signals described in the following sections.
  (let* ((bus (or bus :session))
         (service-name (format "org.kde.StatusNotifierItem-%s-%s"
                               (emacs-pid)
                               ;; FIXME: Technically this should be unique rather than random.
                               (random 65535)))
         (interface-name "org.kde.StatusNotifierItem")
         ;; NOTE: I do not understand D-Bus.  I cannot find a way to get from "org.freedesktop.StatusNotifierWatcher"
         ;; to "org.kde.StatusNotifierWatcher" other than regexp matches against all service names, which is silly.
         (watcher (--first (string-suffix-p ".StatusNotifierWatcher" it)
                           (dbus-list-known-names :session))))
    (oset item service service-name)
    (oset item interface interface-name)
    (oset item watcher service-name)
    ;; FIXME: Check return value of `dbus-register-service'.
    (dbus-call-method bus watcher "/StatusNotifierWatcher" watcher "RegisterStatusNotifierItem" service-name)
    (dbus-status-notifier-item--register-properties bus :service service-name :item item)
    (dbus-status-notifier-item--register-methods bus :service service-name :item item)

    (dbus-register-service bus service-name)

    ;; NOTE: I think we are supposed to SEND these signals, not receive
    ;; them, so I don't think we need to register for these signals.
    ;; (dbus-status-notifier-item--register-signals bus :service service-name :item item)
    ))

(cl-defun dbus-status-notifier-item--register-properties (bus &key service path interface item)
  "Register all properties for ITEM on BUS.
FIXME: Document arguments."
  (let ((path (or path (replace-regexp-in-string (rx ".") "/" service)))
        (interface (or interface service))
        (properties '(("Category" . category)
                      ("Id" . id)
                      ("Title" . title)
                      ("Status" . status)
                      ("WindowId" . window-id)
                      ("IconName" . icon-name)
                      ("IconPixmap" . icon-pixmap)
                      ("OverlayIconName" . overlay-icon-name)
                      ("OverlayIconPixmap" . overlay-icon-pixmap)
                      ("AttentionIconName" . attention-icon-name)
                      ("AttentionIconPixmap" . attention-icon-pixmap)
                      ("AttentionMovieName" . attention-movie-name)
                      ("ToolTip" . tool-tip)
                      ("ItemIsMenu" . item-menu-p)
                      ("Menu" . menu))))
    (cl-loop for (property . slot) in properties
             for value = (when (slot-boundp item slot)
                           (eieio-oref item slot))
             ;; FIXME: I don't know what this means:

             ;; The interface "org.freedesktop.DBus.Properties" is added to PATH, including a
             ;; default handler for the "Get", "GetAll" and "Set" methods of this interface.
             ;; When EMITS-SIGNAL is non-nil, the signal "PropertiesChanged" is sent when the
             ;; property is changed by ‘dbus-set-property’.
             when value
             do (dbus-register-property bus service path interface property :read value))))

(cl-defun dbus-status-notifier-item--register-methods (bus &key service path interface item)
  "Register all methods for ITEM on BUS.
FIXME: Document arguments."
  (let ((path (or path (replace-regexp-in-string (rx ".") "/" service)))
        (interface (or interface service))
        (methods '(("Category" . category)
                   ("Id" . id)
                   ("Title" . title)
                   ("Status" . status)
                   ("WindowId" . window-id)
                   ("IconName" . icon-name)
                   ("IconPixmap" . icon-pixmap)
                   ("OverlayIconName" . overlay-icon-name)
                   ("OverlayIconPixmap" . overlay-icon-pixmap)
                   ("AttentionIconName" . attention-icon-name)
                   ("AttentionIconPixmap" . attention-icon-pixmap)
                   ("AttentionMovieName" . attention-movie-name)
                   ("ToolTip" . tool-tip)
                   ("ItemIsMenu" . item-menu-p)
                   ("Menu" . menu))))
    (cl-loop for (method . slot) in methods
             for handler = (when (slot-boundp item slot)
                             (eieio-oref item slot))
             when handler
             do (dbus-register-method bus service path interface method handler))))

(cl-defun dbus-status-notifier-item--register-signals (bus &key service path interface item)
  "Register all signals for ITEM on BUS and return list of objects.
Objects can be used to remove registrations.  See
`dbus-register-signal'.
FIXME: Document arguments."
  (let ((path (or path (replace-regexp-in-string (rx ".") "/" path)))
        (interface (or interface service))
        (signals '(("NewTitle" . new-title)
                   ("NewIcon" . new-icon)
                   ("NewAttentionIcon" . new-attention-icon)
                   ("NewOverlayIcon" . new-overlay-icon)
                   ("NewToolTip" . new-tool-tip)
                   ("NewStatus" . new-status))))
    (cl-loop for (signal . slot) in signals
             for handler = (when (slot-boundp item slot)
                             (eieio-oref item slot))
             when handler
             collect (dbus-register-signal bus service path interface signal handler))))

(cl-defun dbus-status-notifier-item-status (bus &key item new-status)
  "FIXME"
  (with-slots (service interface watcher status) item
    (setf status new-status)
    (dbus-send-signal bus service "/StatusNotifierWatcher" interface "NewStatus" new-status)))

;;;; Footer

(provide 'dbus-status-notifier)

;;; dbus-status-notifier.el ends here
