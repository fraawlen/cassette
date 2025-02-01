# CGUI Input Swap

TODO

## Table of Contents <a name="toc"></a>

- [Syntax](#syntax)
- [Groups](#groups)
- [Focus](#focus)
- [Accelerators](#accel)
- [Clipboard](#clip)
- [Window Shortcuts](#window)
- [Cell Shortcuts](#cell)
- [App Shortcuts](#app)

## Syntax <a name="syntax"></a>

```
namespace id group:value
```

- namespace is `key` or `button`
- id is `x`, `Mx` or `Sx` with `x` a keycode or button is and, respectively, base input, input + mod and input + mod + shift modifier

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Groups <a name="groups"></a>

Shortcuts can be sorted into the following groups:

| ** Shortcut Group** | **Description ** |
|---------------------|------------------|
| default             | Default |
| none                | Disable input |
| value               | Override input (i.e. `button 2 value:3` will interpret all button 2 presses as button 3 |
| focus               | Focus manipulation |
| accelerator         | Custom app shortcut |
| clipboard_cut       | Cut from a clipboard |
| clipboard_copy      | Copy from a clipboard |
| clipboard_paste     | Paste from a clipboard |
| app                 | Generic app shortcut |
| cell                | Generic cell shortcut |
| window              | Generic window shortcut |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Focus <a name="focus"></a>

| **Shortcut Value** | **Description** |
|-------------------|-----------------|
| unfocus           | Lose focus |
| next              | Move focus to next cell |
| prev              | Move focus to previous cell |
| first             | Move focus to first cell |
| last              | Move focus to last cell |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Accelerators <a name="accel"></a>

A core principle of CGUI is that user customization and theming should not interfere with a developer’s workflow, and vice versa. Just as a developer defines layout and functionality while leaving styling decisions to the user, they can also set application shortcuts while letting the end user choose the exact key binding. To support this, CGUI provides a set of common shortcuts (cut, copy, paste, select, etc.) as well as twelve “accelerators.” These accelerators are limited to twelve because they appear in the X window properties—allowing other programs to discover them—and they are also intended to be displayed in real time on screen.

Valid values : 1-12.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Clipboard <a name="clip"></a>

CGUI provides 3 clipboard, use the shortcut group for the clipboard action and the value to access a specific clipboard.

Valid values: 1-3

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Cell shortcuts <a name="cell"></a>

| **Shortcut Value** | **Description** |
|--------------------|-----------------|
| redraw             | force a redraw of the focused cell |
| select-            | reduce active selection in focused cell |
| select+            | expand active selection in focused cell |
| select_all         | select everything in focused cell |
| unselect           | remove active selection in focused cell |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Window shortcuts <a name="window"></a>

| **Shortcut Value** | **Description** |
|--------------------|-----------------|
| lock_grid          | lock the currently active grid layout (in responsive apps) |
| lock_focus         | lock the focused cell |
| redraw             | force a full window redraw |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## App shortcuts <a name="app"></a>

| **Shortcut Value** | **Description** |
|--------------------|-----------------|
| reconfig           | reload the configuration |
| exit               | exit application |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## Default Settings <a name="defaults"></a>

| **Shortcut Group** | **Shortcut Value** | **Keybind**             |
|--------------------|-------------------|-------------------------|
| accelerator        | 1                 | F1                      |
| accelerator        | 2                 | F2                      |
| accelerator        | 3                 | F3                      |
| accelerator        | 4                 | F4                      |
| accelerator        | 5                 | F5                      |
| accelerator        | 6                 | F6                      |
| accelerator        | 7                 | F7                      |
| accelerator        | 8                 | F8                      |
| accelerator        | 9                 | F9                      |
| accelerator        | 10                | F10                     |
| accelerator        | 11                | F11                     |
| accelerator        | 12                | F12                     |
| focus              | node              | Mod + Esc               |
| focus              | next              | Mod + Tab               |
| focus              | first             | Mod + Home              |
| focus              | last              | Mod + End               |
| focus              | prev              | Mod + Shift + Tab       |
| clipboard_copy     | 1                 | Mod + C                 |
| clipboard_copy     | 2                 | Mod + Shift + C         |
| clipboard_cut      | 1                 | Mod + X                 |
| clipboard_cut      | 2                 | Mod + Shift + X         |
| clipboard_paste    | 1                 | Mod + P                 |
| clipboard_paste    | 2                 | Mod + Shift + P         |
| cell               | redraw            | Mod + Backspace         |
| window             | redraw            | Mod + Shift + Backspace |
| window             | lock_focus        | Mod + L                 |
| window             | lock_grid         | Mod + Shift + L         |
| app                | reconfig          | Mod + R                 |
| app                | exit              | Mod + Q                 |

<div align="right">[ <a href="#toc">back to top</a> ]</div>

