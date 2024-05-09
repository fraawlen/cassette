/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
 * License or (at your option) any later version.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the LGPL for the specific language governing rights and limitations.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program. If not,
 * see <http://www.gnu.org/licenses/>.
 */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifndef DG_CORE_H
#define DG_CORE_H

#include <stdbool.h>
#include <stdint.h>

#include <cairo/cairo.h>
#include <xcb/xcb.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_VERSION "0.2.0"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Opaque struct representing windows.
 */
typedef struct _window_t dg_core_window_t;

/**
 * Opaque struct representing top-level grid layouts.
 */
typedef struct _grid_t dg_core_grid_t;

/**
 * Opaque struct representing individual cells (aka widgets) to be added to grids.
 */
typedef struct _cell_t dg_core_cell_t;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Initialises this module and enables the other functions of this header (unless explicitely stated).
 * Should only be called once until dg_core_reset(). Can be called again after, however all tracked
 * components (windows, grids, and cells) should only be used within the session they have been
 * instantiated in.
 *
 * @param argc        : program's main argc, only used to set WM_COMMAND window property
 * @param argv        : program's main argv, only used to set WM_COMMAND window property, if not provided
 *                      or if argc is set to 0, "dg" will be used
 * @param class_name  : used to set a common WM_CLASS for all windows, if not provided, the program's name
 *                      will instead be used (obtained from argv)
 * @param class_class : used to set a common WM_CLASS for all windows, if not provided , class_name will be
 *                      used instead
 * @param connection  : optional xcb connection to use, if not provided dg_core_init() will open its own
 *                      connection to the X server
 *
 * @error DG_CORE_ERRNO_MEMORY       : internal configuration issues, the module can't initialize
 * @error DG_CORE_ERRNO_CAIRO        : internal configuration issues, the module can't initialize
 * @error DG_CORE_ERRNO_XCB_CRITICAL : critical xcb error, the module can't initialize
 * @error DG_CORE_ERRNO_XCB          : xcb related error, but it is not critical, the module will be
 *                                     initialized but some features may end up broken or missing (window
 *                                     transparency, window properties or leader window)
 */
void dg_core_init(int argc, char *const *argv, const char *class_name, const char *class_class,
                  xcb_connection_t *connection);

/**
 * Resets the module to its initial pre dg_core_init() state and frees all memory internally allocated.
 * If the session's xcb connection has been opened internally, it will be terminated too.
 * Note that it does not destroy windows, grids and cells created during the session as it is assumed they
 * have been destroyed explicitely before dg_core_reset() is called. Non-destroyed windows, grids and
 * cells after dg_core_reset() will become inacessible and their allocated memory is lost.
 * If the environment variable DG_CORE_DEBUG is set, some extra memory coming from cairo and fontconfig will
 * be explicitely freed for debugging and leak tracking purposes.
 * This function can be used when the module is not initialized.
 *
 * @error DG_CORE_ERRNO_XCB : some internal global X components weren't destroyed properly
 */
void dg_core_reset(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Reload all loaded resources and update windows, grid and widgets.
 *
 * @error DG_CORE_ERRNO_XCB : failed to update some window properties
 */
void dg_core_reconfig(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the xcb connection currently used by the module
 * This function can be used when the module is not initialized.
 *
 * @return : the used xcb connection, NULL if DG has not been initialized
 */
xcb_connection_t *dg_core_get_xcb_connection(void);

/**
 * Gets the xcb window id of the leader window.
 * This function can be used when the module is not initialized.
 *
 * @return : the xcb_window_t id of the leader window, 0 if DG has not been initialized
 */
xcb_window_t dg_core_get_xcb_leader_window(void);

/**
 * Checks if the module has been initialized.
 * This function can be used when the module is not initialized.
 *
 * @return : self-explanatory
 */
bool dg_core_is_init(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Each call to this functions increment an internal serial counter then returns it. This function is only
 * valid when the core module has beem initialised and is reset to 0 when the module is reseted.
 * This function exists only to distibute unique identifiers to other independents modules in a centralized
 * and collision free manner.
 *
 * @return self-explanatory
 */
unsigned int dg_core_get_serial(void);

/************************************************************************************************************/
/* LOOP *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Forcefully exit the event loop.
 */
void dg_core_loop_abort(void);

/**
 * Allows the end-user to abort the event loop with a keybind.
 * This is the default state.
 * This function can be used when the module is not initialized. Its state is also maintained between
 * init/reset core sessions.
 */
void dg_core_loop_allow_user_exit(void);

/**
 * Blocks the end-user from aborting the event loop with a keybind.
 * To be used in programs like screenlockers.
 * This function can be used when the module is not initialized. Its state is also maintained between
 * init/reset core sessions.
 */
void dg_core_loop_block_user_exit(void);

/**
 * Enter the event loop. It will run until it is aborted, the session is reseted or if there are no remaining
 * active windows. It should not be called recursively.
 */
void dg_core_loop_run(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds an extra user defined event handler. The given function @fn is called each time a new xcb event is
 * received. It runs after the main internal event handler.
 * It can be used to extend some DG event-based behaviour.
 * Only one event handler can be set at a time. To unset, call this function with fn = NULL.
 * Can be called before, during or after dg_core_loop_run().
 *
 * @param fn : function to set as callback
 */
void dg_core_loop_set_event_postprocessor(void (*fn)(xcb_generic_event_t *x_ev));

/**
 * Adds an extra user defined event handler. The given function @fn is called each time a new xcb event is
 * received. It runs before the main internal event handler. If the function return true then the internal
 * event handler will be skipped for the ongoing event.
 * It can be used to extend or override some DG event-based behaviour.
 * Only one event handler can be set at a time. To unset, call this call this function with fn = NULL.
 * Can be called before, during or after dg_core_loop_run().
 *
 * @param fn : function to set as callback
 */
void dg_core_loop_set_event_preprocessor(bool (*fn)(xcb_generic_event_t *x_ev));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Injects a fake event in the form of an Xmessage to force a run through the event loop. When a signal is
 * received the callback function set by dg_core_loop_set_callback_signal() is called.
 * The sole reason this function exists is to give multithreaded applications an easy way of interacting with
 * DG, since DG itself is not thread safe.
 * Note that this signal does a round trip to the X server, so its effect is not instantaneous and is
 * therefore not suitable for precise time-dependent activities.
 * Exceptionally, due to its main use-case being multi-threading, it does not set a DG error code (as the
 * functions in the errno.h header are not thread safe either).
 *
 * @param serial : number used for signal + callback pairs identification
 *
 * @return : true in case of success, false otherwhise
 */
bool dg_core_loop_send_signal(uint32_t serial);

/**
 * Callback function of dg_core_loop_send_signal(). Called each time a signal is received.
 *
 * @param fn : function to callback
 *
 * @subparam fn.serial : serial identifier of the received signal
 */
void dg_core_loop_set_callback_signal(void (*fn)(uint32_t serial));

/************************************************************************************************************/
/* WINDOW ***************************************************************************************************/
/************************************************************************************************************/

/**
 * Window types.
 */
typedef enum {
	DG_CORE_WINDOW_DEFAULT,
	DG_CORE_WINDOW_FIXED,
} dg_core_window_kind_t;

/**
 * Window state update setting.
 */
typedef enum {
	DG_CORE_WINDOW_SET_STATE,
	DG_CORE_WINDOW_UNSET_STATE,
} dg_core_window_state_setting_mode_t;

/**
 * Window states.
 */
typedef enum {
	DG_CORE_WINDOW_STATE_INITIAL      = 0,
	DG_CORE_WINDOW_STATE_ACTIVE       = 1 << 0, /* DG  */
	DG_CORE_WINDOW_STATE_MAPPED       = 1 << 1, /* X11 */
	DG_CORE_WINDOW_STATE_OBSCURED     = 1 << 2, /* X11 */
	DG_CORE_WINDOW_STATE_FOCUSED      = 1 << 3, /* X11 */
	DG_CORE_WINDOW_STATE_DISABLED     = 1 << 4, /* DG  */
	DG_CORE_WINDOW_STATE_LOCKED_GRID  = 1 << 5, /* DG  */
	DG_CORE_WINDOW_STATE_LOCKED_FOCUS = 1 << 6, /* DG  */
} dg_core_window_state_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates a bare window without any grid or cell. Thereofre it cannot be activated until at least one grid is
 * pushed to it. By default (with the DG_CORE_WINDOW_DEFAULT kind) the window is managed by the window
 * manager. In case its fixed (with the DG_CORE_WINDOW_FIXED kind), then the created window will bypass the
 * window manager and its position and size will need to be explicitely set with
 * dg_core_window_set_fixed_size() and dg_core_window_set_fixed_position(). If these functions are not called,
 * by default, the fixed window will be located at x = 0 and y = 0 with a width and height of 400 pixels.
 *
 * @return : created window pointer, NULL in case of critical, memory or stack error.
 *
 * @error DG_CORE_ERRNO_MEMORY     : out of memory to allocate to the window
 * @error DG_CORE_ERRNO_STACK      : failed to push the grid to the grid tracker
 * @error DG_CORE_ERRNO_CAIRO_CRIT : failed to create a cairo context
 * @error DG_CORE_ERRNO_XCB        : failed to set some window properties but the window is still created
 * @error DG_CORE_ERRNO_XCB_CRIT   : failed to setup necessary X components for a working window
 */
dg_core_window_t *dg_core_window_create(dg_core_window_kind_t kind);

/**
 * Destroys a given window and free memory. Pushed grids are not destroyed.
 *
 * @param w : window to destroy
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the window from the window tracker
 * @error DG_CORE_ERRNO_XCB   : some internal X components may have not been properly freed
 */
void dg_core_window_destroy(dg_core_window_t *w);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Activates and puts a window on the screen
 * This function requires the target window to be deactivated.
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window was not properly mapped
 */
void dg_core_window_activate(dg_core_window_t *w);

/**
 * Removes a window from the screen, and deactivates it.
 * This function requires the target window to be active.
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window was not properly unmapped
 */
void dg_core_window_deactivate(dg_core_window_t *w);

/**
 * Add extra width or height (in characters) to a window, independently of the default grid. Negative width
 * or height will be interpreted as inverted height-width character dimensions, see
 * dg_core_config_convert_str_width() and dg_core_config_convert_str_height().
 * This function is allowed to be used only on default type windows, and will fail an assert if the given
 * window is of the fixed kind.
 * It will only take effect when the window does not have a currently active grid (in other words the window
 * has not been yet activated or its current grid was reseted). If the width or height (or both) are not
 * flexible this function will have no effect on the affected axis. It may also not be respected by the window
 * manager.
 * As a side effect, upon window activation, if a bigger grid than the default one fits thanks to the
 * increased size, the bigger one will be presented instead.
 *
 * @param w        : target window
 * @param cw_extra : extra width  (in amount of horizontal chars)
 * @param ch_extra : extra height (in amount of vertical chars)
 */
void dg_core_window_set_extra_size(dg_core_window_t *w, int16_t cw_extra, int16_t ch_extra);

/**
 * Sets the width and height (in pixels) of a window. Negative or zero pw and ph values are not allowed.
 * This function is also only allowed to be used on fixed type windows, and will fail an assert if the given
 * window is of the default kind.
 *
 * @param w  : target window
 * @param pw : width  (in pixels)
 * @param ph : height (in pixels)
 * 
 * @error DG_CORE_ERRNO_XCB : the window was not properly resized
 */
void dg_core_window_set_fixed_size(dg_core_window_t *w, int16_t pw, int16_t ph);

/**
 * Sets the position (in pixels) of the window on the screen, regardless of the monitors positions.
 * This function is only allowed to be used on fixed type windows, and will fail an assert if the given
 * window is of the default kind.
 *
 * @param w  : target window
 * @param px : x position (in pixels)
 * @param px : y position (in pixels)
 *
 * @error DG_CORE_ERRNO_XCB : the window was not properly positioned
 */
void dg_core_window_set_fixed_position(dg_core_window_t *w, int16_t px, int16_t py);

/**
 * Marks a window as enabled, and allow user input (default state).
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_enable(dg_core_window_t *w);

/**
 * Marks a window as disabled, and block user input until it is enabled back.
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_disable(dg_core_window_t *w);

/**
 * Puts the window out of an urgent state. See dg_core_window_urgent().
 * This function requires the target window to be active.
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_non_urgent(dg_core_window_t *w);

/**
 * Schedules a window to be completely redrawn for the next frame.
 * Overrides dg_core_cell_redraw().
 * This function requires the target window to be active.
 *
 * @param w : target window
 */
void dg_core_window_redraw(dg_core_window_t *w);

/**
 * Sets the window title when it is in a normal or iconified state.
 * If name = NULL then this parameter will default to the class name set during dg_core_session_init().
 * If name_icon = NULL, then this parameter will default to the name value.
 *
 * @param w         : target window
 * @param name      : window's name to set
 * @param name_icon : window's name to set when its iconified
 *
 * @error DG_CORE_ERRNO_XCB : the window X name could not be updated properly
 */
void dg_core_window_rename(dg_core_window_t *w, const char *name, const char *name_icon);

/**
 * Sets up one of the 12 window-wide shortcuts. When a windows receives an accelerator trigger event
 * (expected to be F1-F12, but it actually depends on the end user config) fn is called.
 * Passing NULL to fn unsets the accelerator (and in this case the param name does not matter and can be
 * set to NULL too). The accelerator name becomes visible for external programs through X properties.
 *
 * @param w        : target window
 * @param accel_id : id of the accelerator (1..12, any other values are not allowed)
 * @param name     : name of the accelerator
 * @param fn       : function to callback when the accelerator is triggered
 *
 * @subparam fn.w        : window that received the accelerator event
 * @subparam fn.accel_id : id of the triggered accelerator
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_set_accelerator(dg_core_window_t *w, int accel_id, const char *name,
                                    void (*fn)(dg_core_window_t *w, int accel_id));

/**
 * Makes the window w transient to the window w_for. If window hints are respected by the end-user's WM, then
 * that would force w to always sit in front of w_for. If w_for = NULL then any w transience is unset.
 *
 * @param w     : target window
 * @param w_for : window to be transient for
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_transient_for(dg_core_window_t *w, dg_core_window_t *w_for);

/**
 * Puts the window in an urgent state and will stay in it until the window gets focused by the end-user or 
 * dg_core_window_non_urgent() is called. If the window was already focused by the end-user when this function
 * is called, it will have no effect.
 * This function requires the target window to be active.
 *
 * @param w : target window
 *
 * @error DG_CORE_ERRNO_XCB : the window X properties could not be updated correctly
 */
void dg_core_window_urgent(dg_core_window_t *w);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds a grid to the target window.
 * The given grid should not be already part of the window when this function is called.
 * This function requires the target window to be deactivated.
 * On internal error nothing is modified.
 *
 * @param w : target window
 * @param g : grid to pull
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the grid from the window
 * @error DG_CORE_ERRNO_XCB   : failed to update X window hints
 */
void dg_core_window_pull_grid(dg_core_window_t *w, dg_core_grid_t *g);

/**
 * Removes a grid from the target window.
 * The given grid should be part of the window when this function is called.
 * The added grid should be compatible with the grids already in place. See dg_core_window_test_grid_push().
 * This function requires the target window to be deactivated.
 * On internal error nothing is modified.
 *
 * @param w : target window
 * @param g : grid to push
 *
 * @error DG_CORE_ERRNO_STACK : failed to push the grid to the window
 * @error DG_CORE_ERRNO_XCB   : failed to update X window hints
 */
void dg_core_window_push_grid(dg_core_window_t *w, dg_core_grid_t *g);

/**
 * Swap p1 with p2 on the target window.
 * p1 should be part of the window when this function is called.
 * The 2 grids should be compatible. See dg_core_window_test_grid_swap().
 *
 * @param w  : target window
 * @param g1 : grid to swap out
 * @param g2 : grid to swap in
 */
void dg_core_window_swap_grid(dg_core_window_t *w, dg_core_grid_t *g1, dg_core_grid_t *g2);

/**
 * Sets a default grid that will be shown upon first window activation or after a grid reset.
 * The given grid should have already been pushed to the window when this function is called.ven
 * This function requires the target window to be deactivated.
 *
 * @param w : target window
 * @param g : grid to set as default
 */
void dg_core_window_set_default_grid(dg_core_window_t *w, dg_core_grid_t *g);

/**
 * Reset the current grid to the default.
 * This function requires the target window to be deactivated.
 *
 * @param w : target window
 */
void dg_core_window_reset_current_grid(dg_core_window_t *w);

/**
 * Checks if the grids for the grid push operation are compatible.
 * The grid should not be part of the window when this function is called.
 *
 * @param w : target window
 * @param g : grid to push
 *
 * @return : true if the grid is compatible with those previously pushed to the window, false otherwise
 */
bool dg_core_window_test_grid_push(dg_core_window_t *w, dg_core_grid_t *g);

/**
 * Checks if the grids for the grid swap operation are compatible.
 * g2 should not be part of the window, while g1 needs to be when this function is called.
 *
 * @param w  : target window
 * @param g1 : grid to swap out
 * @param g2 : grid to swap in
 *
 * @return : true if the grids are compatible, false otherwise
 */
bool dg_core_window_test_grid_swap(dg_core_window_t *w, dg_core_grid_t *g1, dg_core_grid_t *g2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets a callback function that is called when the target window receives a X close message request. If no
 * callback is set (default), the window will just automatically deactivate and trigger a state callback
 * (if set). Otherwhise the given callback function is called and window deactivation should be done on the
 * programmer's side manually.
 * Call with fn = NULL to unset the callback.
 *
 * @param w  : target window
 * @param fn : function to callback
 *
 * @subparam fn.w : the window that got a close request
 */
void dg_core_window_set_callback_close(dg_core_window_t *w, void (*fn)(dg_core_window_t *w));

/**
 * Sets a callback function that is called when the focused cell on the target window changes.
 * Call with fn = NULL to unset the callback.
 *
 * @param w  : target window
 * @param fn : function to callback
 *
 * @subparam fn.w : the window whose focused cell changed
 * @subparam fn.c : the cell that is now focused, if NULL then no cells are currently focused for fn.w
 */
void dg_core_window_set_callback_focus(dg_core_window_t *w, void (*fn)(dg_core_window_t *w,
                                       dg_core_cell_t *c));

/**
 * Sets a callback function that is called when window's currently presented grid is switched to another one.
 * Call with fn = NULL to unset the callback.
 *
 * @param w  : target window
 * @param fn : function to callback
 *
 * @subparam fn.w : window who had its currently presented grid swapped
 * @subparam fn.p : the grid that is now currently presented for fn.w
 */
void dg_core_window_set_callback_grid(dg_core_window_t *w, void (*fn)(dg_core_window_t *w,
                                      dg_core_grid_t *g));

/**
 * Sets a callback function that is called when the bitfield representing the window state is modified, see
 * dg_core_window_states_t.
 * Call with fn = NULL to unset the callback.
 *
 * @param w  : target window
 * @param fn : function to callback

 * @subparam fn.w          : window whose state state was modified
 * @subparam fn.state_bits : the exact bits that were modified, to get the actual window state, see
 *                           dg_core_window_get_state()
 * @subparam fn.mode       : see dg_core_window_state_setting_modes_t
 */
void dg_core_window_set_callback_state(dg_core_window_t *w, void (*fn)(dg_core_window_t *w,
                                       dg_core_window_state_t state,
                                       dg_core_window_state_setting_mode_t mode));

/**
 * Sets a callback function that is called everytime the visuals of the window get updated. Allows to draw
 * extra overlays on top of a given DG window.
 *
 * @param w  : target window
 * @param fn : function to callback
 *
 * @subparam fn.w     : window that had its visuals updated
 * @subparam fn.delay : amount of microseconds since last update
 */
void dg_core_window_set_callback_redraw(dg_core_window_t *w, void (*fn)(dg_core_window_t *w,
                                        unsigned long delay));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the window's cairo context for drawing.
 *
 * @param w : target window
 *
 * @return : self-explanatory
 */
cairo_t *dg_core_window_get_cairo_context(dg_core_window_t *w);

/**
 * Gets the window's cairo surface for drawing.
 *
 * @param w : target window
 *
 * @return : self-explanatory
 */
cairo_surface_t *dg_core_window_get_cairo_surface(dg_core_window_t *w);

/**
 * Gets the grid currently presented of the window.
 *
 * @param w : target window
 *
 * @return : self-explanatory
 */
dg_core_grid_t *dg_core_window_get_current_grid(dg_core_window_t *w);

/**
 * Return the current height of the given window in pixels
 *
 * @param w L target window
 *
 * @return : self-explanatory
 */
int16_t dg_core_window_get_pixel_height(dg_core_window_t *w);

/**
 * Return the current width of the given window in pixels
 *
 * @param w L target window
 *
 * @return : self-explanatory
 */
int16_t dg_core_window_get_pixel_width(dg_core_window_t *w);

/**
 * Gets the state of the window, see dg_core_window_state_t. Its a bitfield.
 *
 * @param w : target window
 *
 * @return : self-explanatory
 */
dg_core_window_state_t dg_core_window_get_state(dg_core_window_t *w);

/**
 * Gets the window's X window identifier.
 *
 * @param w : target window
 *
 * @return : self-explanatory
 */
xcb_window_t dg_core_window_get_xcb_window(dg_core_window_t *w);

/************************************************************************************************************/
/* GRID ****************************************************************************************************/
/************************************************************************************************************/

/*
 * Grid flexibility comparison results
 */
typedef enum {
	DG_CORE_GRID_FLEX_SAME,
	DG_CORE_GRID_FLEX_DIFFERENT,
} dg_core_grid_test_flex_result_t;

/*
 * Grid size comparison results
 */
typedef enum {
	DG_CORE_GRID_SIZE_EQUAL,
	DG_CORE_GRID_SIZE_BIGGER,
	DG_CORE_GRID_SIZE_SMALLER,
	DG_CORE_GRID_SIZE_UNDEFINED,
} dg_core_grid_test_size_result_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates a grid with identical column and rows as well as cells assignments.
 * Useful for creating multiple layouts sharing the same base and swap them using dg_core_window_swap_grid()
 * to create dynamic layouts.
 *
 * @param g : grid to clone
 *
 * @return : created grid pointer, NULL in case of error
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate to the grid and areas
 * @error DG_CORE_ERRNO_STACK  : failed to push the new grid to the grid area trackers
 */
dg_core_grid_t *dg_core_grid_clone(dg_core_grid_t *g);

/**
 * Creates a grid with cw rows and ch columns. Initially is has no cells and all its rows and columns have a
 * size of 1 (in characters) and no flexibility.
 *
 * @param cw : amount of columns
 * @param ch : amount of rows
 *
 * @return : created grid pointer, NULL in case of error
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate to the grid
 * @error DG_CORE_ERRNO_STACK  : failed to push the new grid to the grid tracker
 */
dg_core_grid_t *dg_core_grid_create(int16_t cw, int16_t ch);

/**
 * Destroys a given grid and free memory. Assigned cells are not destroyed.
 *
 * @param g : grid to destroy
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the grid from the grid tracker
 */
void dg_core_grid_destroy(dg_core_grid_t *g);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Assigns a cell to a rectangular area of the grid.
 * The assigned area should be within the bounds of the grid.
 * The given grid should not be part of any window when this function is called.
 * On error nothing is modified.
 *
 * @param g  : target grid
 * @param c  : cell to be assigned to a grid
 * @param cx : x position of the cell (in columns)
 * @param cy : y position of the cell (in rows)
 * @param cw : width  of the cell (in columns)
 * @param ch : height of the cell (in rows)
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate for a grid's area
 * @error DG_CORE_ERRNO_STACK  : could not push the area to the grid
 */
void dg_core_grid_assign_cell(dg_core_grid_t *g, dg_core_cell_t *c, int16_t cx, int16_t cy, int16_t cw,
                              int16_t ch);

/**
 * Sets the growth factor of a given column. If it's set at 0, the column will not stretch with the window.
 * If no columns can grow, the grid, and the window its part of, will have a fixed width.
 * Negative values are invalid.
 * The given grid should not be part of any window when this function is called.
 *
 * @param g      : target grid
 * @param cx     : position of the column, starts at 0
 * @param growth : column growth factor
 */
void dg_core_grid_set_column_growth(dg_core_grid_t *g, int16_t cx, double growth);

/**
 * Sets the character width of a given column. Negative values are possible, and will be interpreted as
 * inverted height-width character dimensions, see dg_core_config_convert_str_width().
 * The given grid should not be part of any window when this function is called.
 *
 * @param g   : target grid
 * @param cx  : position of the column, starts at 0
 * @param cwu : column width, represented by the amound of mono characters that can fit horizontaly
 */
void dg_core_grid_set_column_width(dg_core_grid_t *g, int16_t cx, int16_t cwu);

/**
 * Sets the growth factor of a given row. If it's set at 0, the row will not stretch with the window.
 * If no rows can grow, the grid, and the window its part of, will have a fixed height.
 * Negative values are invalid.
 * The given grid should not be part of any window when this function is called.
 *
 * @param g      : target grid
 * @param cy     : position of the row, starts at 0
 * @param growth : row growth factor
 */
void dg_core_grid_set_row_growth(dg_core_grid_t *g, int16_t cy, double growth);

/**
 * Sets the character height of a given row. Negative values are possible, and will be interpreted as
 * inverted height-width character dimensions, see dg_core_config_convert_str_height().
 * The given grid should not be part of any window when this function is called.
 *
 * @param g   : target grid
 * @param cy  : position of the row, starts at 0
 * @param chu : row height, represented by the amound of mono characters that can fit verticaly
 */
void dg_core_grid_set_row_height(dg_core_grid_t *g, int16_t cy, int16_t chu);

/**
 * Sets a reference grid to a given grid. When some window that has p as current grid changes to another
 * grid due to a window resize action, p will be automatically swapped to p_ref. Used to create a default
 * grid for a given size. That also means p and p_ref should be of equal size and they should have the same
 * flexibility, see dg_core_grid_test_size() and dg_core_grid_test_flexibility() for more details.
 * Setting p_ref to NULL removes the reference grid from p.
 *
 * @param g     : grid to apply a reference to
 * @param g_ref : grid to be referenced
 */
void dg_core_grid_set_reference(dg_core_grid_t *g, dg_core_grid_t *g_ref);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Compares the flexibility of 2 grids.
 * The given result is relative to p1.
 *
 * @param g1 : grid to test for
 * @param g2 : grid to test against
 *
 * @return : self-explanatory
 */
dg_core_grid_test_flex_result_t dg_core_grid_test_flexibility(dg_core_grid_t *g1, dg_core_grid_t *g2);

/**
 * Compares the size of 2 grids. In this context, the "size" is interpreted in DG terms, aka, if a grid is
 * always bigger/smaller/equal to another indifferently of the geometric values in the config. If said
 * certainity could not be established, an undefined size value is returned.
 * The given result is relative to p1.
 *
 * @param g1 : grid to test for
 * @param g2 : grid to test against
 *
 * @return : self-explanatory
 */
dg_core_grid_test_size_result_t dg_core_grid_test_size(dg_core_grid_t *g1, dg_core_grid_t *g2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Calculates the minimum width (in pixels) of a given grid according to the config values when this function
 * is called, meaning that if the configuration changes, the returned value may have changed too. If this
 * function is used to set the size of a fixed window to be that of a panel, consider tracking the
 * configuration changes with a callback set with dg_core_resource_set callback().
 * It should be noted that the maximum size of a grid is either the minimum value returned here if the given
 * grid is not flexible, or INT16_MAX otherwhise.
 *
 * @param : target grid
 */
int16_t dg_core_grid_get_min_pixel_width(dg_core_grid_t *g);

/**
 * Calculates the minimum height (in pixels) of a given grid according to the config values when this function
 * is called, meaning that if the configuration changes, the returned value may have changed too. If this
 * function is used to set the size of a fixed window to be that of a panel, consider tracking the
 * configuration changes with a callback set with dg_core_resource_set callback().
 * It should be noted that the maximum size of a grid is either the minimum value returned here if the given
 * grid is not flexible, or INT16_MAX otherwhise.
 *
 * @param : target grid
 */
int16_t dg_core_grid_get_min_pixel_height(dg_core_grid_t *g);

/************************************************************************************************************/
/* CELL *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Cell focus level. Secondary focuses only happens with multitouch and do not generate focus events.
 */
typedef enum {
	DG_CORE_CELL_FOCUS_NONE = 0,
	DG_CORE_CELL_FOCUS_SECONDARY,
	DG_CORE_CELL_FOCUS_PRIMARY,
	DG_CORE_CELL_FOCUS_PRIMARY_LOCKED,
} dg_core_cell_focus_t;

/**
 * Subfocus action type, intented for meta-cells to update the internal focus before a top-level focus change.
 */
typedef enum {
	DG_CORE_CELL_SUBFOCUS_LEFT,
	DG_CORE_CELL_SUBFOCUS_RIGHT,
	DG_CORE_CELL_SUBFOCUS_UP,
	DG_CORE_CELL_SUBFOCUS_DOWN,
	DG_CORE_CELL_SUBFOCUS_LEFTMOST,
	DG_CORE_CELL_SUBFOCUS_RIGHTMOST,
	DG_CORE_CELL_SUBFOCUS_TOP,
	DG_CORE_CELL_SUBFOCUS_BOTTOM,
	DG_CORE_CELL_SUBFOCUS_NEXT,
	DG_CORE_CELL_SUBFOCUS_PREV,
	DG_CORE_CELL_SUBFOCUS_FIRST,
	DG_CORE_CELL_SUBFOCUS_LAST,
} dg_core_cell_subfocus_t;

/**
 * Return messages in response to cell drawing requests.
 */
typedef enum {
	DG_CORE_CELL_DRAW_MSG_NONE           = 0,
	DG_CORE_CELL_DRAW_MSG_REQUEST_UPDATE = 1U << 0,
	DG_CORE_CELL_DRAW_MSG_OUT_OF_BOUNDS  = 1U << 1,
} dg_core_cell_draw_msg_t;

/**
 * Return messages in response to cell events.
 */
typedef enum {
	DG_CORE_CELL_EVENT_MSG_NONE           = 0,
	DG_CORE_CELL_EVENT_MSG_REJECT         = 1U << 0,
	DG_CORE_CELL_EVENT_MSG_REQUEST_UPDATE = 1U << 1,
	DG_CORE_CELL_EVENT_MSG_REQUEST_LOCK   = 1U << 2,
	DG_CORE_CELL_EVENT_MSG_REQUEST_UNLOCK = 1U << 3,
} dg_core_cell_event_msg_t;

/**
 *
 */
typedef enum {
	DG_CORE_CELL_EVENT_NONE = 0,
	DG_CORE_CELL_EVENT_ASSIGN,
	DG_CORE_CELL_EVENT_CANCEL,
	DG_CORE_CELL_EVENT_STATE_ENABLE,
	DG_CORE_CELL_EVENT_STATE_DISABLE,
	DG_CORE_CELL_EVENT_KEY_PRESS,
	DG_CORE_CELL_EVENT_KEY_RELEASE,
	DG_CORE_CELL_EVENT_BUTTON_PRESS,
	DG_CORE_CELL_EVENT_BUTTON_RELEASE,
	DG_CORE_CELL_EVENT_POINTER_HOVER,
	DG_CORE_CELL_EVENT_POINTER_DRAG,
	DG_CORE_CELL_EVENT_TOUCH_BEGIN,
	DG_CORE_CELL_EVENT_TOUCH_END,
	DG_CORE_CELL_EVENT_TOUCH_UPDATE,
	DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_ACTION,
	DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_POINTER,
	DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_TOUCH,
	DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE,
	DG_CORE_CELL_EVENT_FOCUS_LOSE,
	DG_CORE_CELL_EVENT_FOCUS_LOCK,
	DG_CORE_CELL_EVENT_FOCUS_UNLOCK,
	DG_CORE_CELL_EVENT_CLIPBOARD_CUT,
	DG_CORE_CELL_EVENT_CLIPBOARD_COPY,
	DG_CORE_CELL_EVENT_CLIPBOARD_PASTE,
	DG_CORE_CELL_EVENT_SELECT_LESS,
	DG_CORE_CELL_EVENT_SELECT_MORE,
	DG_CORE_CELL_EVENT_SELECT_NONE,
	DG_CORE_CELL_EVENT_SELECT_ALL,
	DG_CORE_CELL_EVENT_SUBFOCUS,
	DG_CORE_CELL_EVENT_SEEK_CELL,
	DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL,
	DG_CORE_CELL_EVENT_WINDOW_ENABLE,
	DG_CORE_CELL_EVENT_WINDOW_DISABLE,
	DG_CORE_CELL_EVENT_TRIGGER,
	DG_CORE_CELL_EVENT_CUSTOM,
} dg_core_cell_event_kind_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * This struct provides all the necessary data for cells to draw themselves.
 * The msg field is intended to be modified by the cell drawing function as a response to the drawing request.
 *
 * @param msg            : to be modified by the cell event handler, is used as return value by the caller
 * @param focus          : cell's focus level at the time of drawing
 * @param delay          : time in microseconds since last draw, may be set to 0 for initial states
 * @param cell_px        : cell's x pixel position on the window
 * @param cell_py        : cell's y pixel position on the window
 * @param cell_pw        : cell's pixel width
 * @param cell_ph        : cell's pixel height
 * @param is_enabled     : cell state
 * @param win_is_enabled : window enable state
 * @param c_ctx          : cairo context to draw on
 */
typedef struct {
	dg_core_cell_draw_msg_t msg;
	dg_core_cell_focus_t focus;
	unsigned long delay;
	int16_t cell_px;
	int16_t cell_py;
	int16_t cell_pw;
	int16_t cell_ph;
	bool is_enabled;
	bool win_is_enabled;
	cairo_t *c_ctx;	
} dg_core_cell_drawing_context_t;

/**
 * Large structure holding all the necessary information to catch and process cell events.
 * The msg field is intended to be modified by the cell event handler as a response to the event.
 * Some events (DG_CORE_CELL_EVENT_INFO_* kinds) also expect their fields to be modified by the
 * cell event handler as a mechanism of sending data from the cell receiving the event, to the event
 * creator / caller.
 * It relies on C11 anonymous unions and structs to set its kind dependent fields.
 * Only use kind dependents fields after checking the kind.
 *
 * @param kind            : event type
 * @param msg             : to be modified by the cell event handler, is used as return value by the caller
 * @param focus           : cell's focus level at the time of the event
 * @param w_host          : window on which the cell is
 * @param cell_px         : cell's x pixel position on the window
 * @param cell_py         : cell's y pixel position on the window
 * @param cell_pw         : cell's pixel width
 * @param cell_ph         : cell's pixel height
 * @param is_enabled     : cell state
 * @param win_is_enabled : window enable state
 * @param key_code        : X raw keycode
 * @param key_sym         : X keysym
 * @param keu_utf32       : utf32 encoding of an unicode codepoint, converted from keysym
 * @param key_utf8        : 4 chars array representing an unicode codepoint in utf8 encoding
 * @param button_id       : pointer button identifier (1..12)
 * @param button_px       : pointer x pixel position (relative to window) at the time of event
 * @param button_py       : pointer y pixel position (relative to window) at the time of event
 * @param button_n        : amount of pointer buttons being pressed at the time of event (including event's)
 * @param pointer_px      : pointer x pixel position (relative to window) at the time of event
 * @param pointer_py      : pointer y pixel position (relative to window) at the time of event
 * @param touch_id        : touch unique identifier
 * @param touch_px        : touch x pixel position (relative to window) at the time of event
 * @param touch_py        : touch y pixel position (relative to window) at the time of event
 * @param touch_dpx       : touch delta x pixel movement since last touch event
 * @param touch_dpy       : touch delta y pixel movement since last touch event
 * @param touch_n         : amount of active touches at the time of event (including event's)
 * @param focus_px        : pointer x pixel position (relative to window) at the time of event
 * @param focus_py        : pointer y pixel position (relative to window) at the time of event
 * @param focus_prev_px   : x pixel position (relative to window) of the previously focused cell
 * @param focus_prev_py   : y pixel position (relative to window) of the previously focused cell
 * @param focus_cell      : cell to focus
 * @param seek_cell       : cell to find
 * @param info_focus_cell : cell that is focused within the cell handling the event if any
 * @param info_focus_px   : x pixel position (relative to window) of the focus within the cell
 * @param info_focus_py   : y pixel position (relative to window) of the focus within the cell
 * @param info_focus_pw   : pixel width of the focus within the cell
 * @param info_focus_ph   : pixel height of the focus within the cell
 * @param subfocus        : subfocus action to execute
 * @param clipboard       : clipboard identifier (1..3)
 * @param trigger         : trigger identifier (1..5)
 * @param custom_id       : arbitrary serial identifier
 * @param custom_data     : arbitrary pointer to data
 * @param custom_data_n   : arbitrary size of data pointed to
 */
typedef struct {
	dg_core_cell_event_kind_t kind;
	dg_core_cell_event_msg_t msg;
	dg_core_cell_focus_t focus;
	dg_core_window_t *w_host;
	int16_t cell_px;
	int16_t cell_py;
	int16_t cell_pw;
	int16_t cell_ph;
	bool is_enabled;
	bool win_is_enabled;
	union {	
		/* DG_CORE_CELL_EVENT_KEY_PRESS   */
		/* DG_CORE_CELL_EVENT_KEY_RELEASE */
		struct {
			uint8_t key_code;
			uint32_t key_sym;
			uint32_t key_utf32;
			char key_utf8[8];
		};
		/* DG_CORE_CELL_EVENT_BUTTON_PRESS   */
		/* DG_CORE_CELL_EVENT_BUTTON_RELEASE */
		struct {
			uint8_t button_id;
			int16_t button_px;
			int16_t button_py;
			size_t button_n;
		};
		/* DG_CORE_CELL_EVENT_POINTER_HOVER */
		/* DG_CORE_CELL_EVENT_POINTER_DRAG  */
		struct {
			int16_t pointer_px;
			int16_t pointer_py;
		};
		/* DG_CORE_CELL_EVENT_TOUCH_BEGIN  */
		/* DG_CORE_CELL_EVENT_TOUCH_END    */
		/* DG_CORE_CELL_EVENT_TOUCH_UPDATE */
		struct {
			uint32_t touch_id;
			int16_t touch_px;
			int16_t touch_py;
			int16_t touch_dpx;
			int16_t touch_dpy;
			size_t touch_n;
		};
		/* DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_POINTER */
		/* DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_TOUCH   */
		struct {
			int16_t focus_px;
			int16_t focus_py;
		};
		/* DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_ACTION */
		struct {
			dg_core_cell_subfocus_t focus_subfocus;
			int16_t focus_prev_px;
			int16_t focus_prev_py;
		};
		/* DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE */
		dg_core_cell_t *focus_cell;
		/* DG_CORE_CELL_EVENT_SEEK_CELL */
		dg_core_cell_t *seek_cell;
		/* DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL */
		struct {
			dg_core_cell_t *info_focus_cell;
			int16_t info_focus_px;
			int16_t info_focus_py;
			int16_t info_focus_pw;
			int16_t info_focus_ph;
		};
		/* DG_CORE_CELL_EVENT_SUBFOCUS*/
		dg_core_cell_subfocus_t subfocus;
		/* DG_CORE_CELL_EVENT_CLIPBOARD_CUT   */
		/* DG_CORE_CELL_EVENT_CLIPBOARD_COPY  */
		/* DG_CORE_CELL_EVENT_CLIPBOARD_PASTE */
		int clipboard;
		/* DG_CORE_CELL_EVENT_TRIGGER */
		int trigger;
		/* DG_CORE_CELL_EVENT_CUSTOM */
		struct {
			unsigned int custom_id;
			void *custom_data;
			size_t custom_data_n;
		};
		/* DG_CORE_CELL_EVENT_NONE            */
		/* DG_CORE_CELL_EVENT_ASSIGN          */
		/* DG_CORE_CELL_EVENT_CANCEL          */
		/* DG_CORE_CELL_EVENT_STATE_ENABLE    */
		/* DG_CORE_CELL_EVENT_STATE_DISABLE   */
		/* DG_CORE_CELL_EVENT_FOCUS_LOSE      */
		/* DG_CORE_CELL_EVENT_FOCUS_LOCK      */
		/* DG_CORE_CELL_EVENT_FOCUS_UNLOCK    */
		/* DG_CORE_CELL_EVENT_SELECT_LESS     */
		/* DG_CORE_CELL_EVENT_SELECT_MORE     */
		/* DG_CORE_CELL_EVENT_SELECT_NONE     */
		/* DG_CORE_CELL_EVENT_SELECT_ALL      */
		/* DG_CORE_CELL_EVEBT_WINDOW_ENABLE   */
		/* DG_CORE_CELL_EVEBT_WINDOW_DISABLE  */
		/* no sub fields for these types      */
	};
} dg_core_cell_event_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates a custom cell and sets it different handlers.
 *
 * @param serial     : abitrary number to be used for cell identification
 * @param fn_draw    : function that draws the cell
 * @parma fn_event   : optional, function that process incoming events, if none is supplied, the cell is
 *                     considered to be passive and cannot be focused by the end user
 * @param fn_destroy : optional, function that is called when a cell is destroyed, usually needed to cleanup
 *                     and free cell's properties
 * @param props      : optional, pointer to an arbitrary chunk of memory, usually used to store a cell's
 *                     internal states for its operation
 *
 * @subparam fn_draw.c    : cell to render
 * @subparam fn_draw.dc   : drawing context, it is statically allocated and should not be freed.
 * @subparam fn_event.c   : cell to process events for
 * @subparam fn_event.ev  : event to process. It is statically allocated and should not be freed.
 * @subparam fn_destroy.c : cell to destroy
 *
 * @return : created cell, NULL in case of failure
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate to the cell
 * @error DG_CORE_ERRNO_STACK  : failed to push the new cell to the cell tracker
 */
dg_core_cell_t *dg_core_cell_create(unsigned int serial,
                                    void (*fn_draw)(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc),
                                    void (*fn_event)(dg_core_cell_t *c, dg_core_cell_event_t *ev),
                                    void (*fn_destroy)(dg_core_cell_t *c),
                                    void *props);

/**
 * Destroys a given cell and free memory.
 *
 * @param c : cell to destroy
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the cell from the cell tracker
 */
void dg_core_cell_destroy(dg_core_cell_t *c);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Disables a cell. A disabled cell cannot be focused by the end user and cannot be actively interacted with.
 * This function has no effect on passive cells without an event processing function.
 *
 * @param c : target cell
 */
void dg_core_cell_disable(dg_core_cell_t *c);

/**
 * Enables a cell. This is also the default cell state.
 * This function has no effect on passive cells without an event processing function.
 *
 * @param c : target cell
 */
void dg_core_cell_enable(dg_core_cell_t *c);

/**
 * Schedules a cell to be redrawn for the next frame. Applies for every areas that hold the given cell on all
 * active and visible windows current grids.
 * Does not override dg_core_window_request_redraw().
 *
 * @param c : target cell
 */
void dg_core_cell_redraw(dg_core_cell_t *c);

/**
 * Enables or disables a cell. See dg_core_cell_enable() and dg_core_cell_disable().
 *
 * @param c : target cell
 */
void dg_core_cell_toggle(dg_core_cell_t *c);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * In case the built-in events are not sufficient, this helper function helps send a custom event to a given
 * cell. The event is propagated to all windows that hold the cell on their current grid.
 */
bool dg_core_cell_send_custom_event(dg_core_cell_t *c, unsigned int id, void *data, size_t data_n);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks if the given cell is enabled or not.
 *
 * @param c : target cell
 *
 * @return : self-explanatory
 */
bool dg_core_cell_is_enabled(dg_core_cell_t *c);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets a cell's associated serial that was set during its creation
 *
 * @return : self-explanatory
 */
unsigned int dg_core_cell_get_serial(dg_core_cell_t *c);

/**
 * Retrieves the internal rendering function of a cell.
 * Normally it is only used within meta-cells so that they could control their childrens.
 *
 * @param fn_render : function that draws the cell
 *
 * @subparam fn_draw.c   : cell to render
 * @subparam fn_draw.dcf : drawing context
 *
 * @return : self-explanatory
 */
void (*dg_core_cell_get_fn_draw(dg_core_cell_t *c))(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

/**
 * Retrieves the internal destroy function of a cell.
 * Normally it is only used within meta-cells so that they could control their childrens.
 *
 * @param fn_destroy : optional, function that is called when a cell is destroyed, usually needed to cleanup
 *                     and free cell's internal properties.
 *
 * @subparam fn_destroy.c : cell to destroy
 *
 * @return : self-explanatory
 */
void (*dg_core_cell_get_fn_destroy(dg_core_cell_t *c))(dg_core_cell_t *c);

/**
 * Retrieves the internal event handler function of a cell.
 * Normally it is only used within meta-cells so that they could control their childrens.
 *
 * @parma fn_event : optional, function that process incoming events, if none is supplied, the cell is
 *                   considered to be passive
 *
 * @subparam  fn_event.c  : cell to process events for
 * @subparam  fn_event.ev : event to process. It is statically allocated and should not be freed.
 *
 * @return : self-explanatory
 */
void (*dg_core_cell_get_fn_event(dg_core_cell_t *c))(dg_core_cell_t *c, dg_core_cell_event_t *ev);

/**
 * Retrieves the internal properties of a cell. Its up to the caller to properly cast the returned pointer
 * to a usable and correct type.
 * Normally only used within cells themselves to retrieve their own properties and saved states.
 *
 * @return : self-explanatory
 */
const void *dg_core_cell_get_props(dg_core_cell_t *c);

/************************************************************************************************************/
/* CLIPBOARD ************************************************************************************************/
/************************************************************************************************************/

/**
 * Acquire ownership of a given clipboard (1..3, respectively CLIPBOARD, PRIMARY_SELECTION and
 * SECONDARY_SELECTION) and copy the given string into an internal buffer. The called of this function can
 * provide two callbacks that will be triggered evertime the clipboard content's are pasted elsewhere or if
 * ownership get lost.
 * This function currently support C strings as copy-pastable data and expects them to be null terminated.
 *
 * @param clipboard : target clipboard (1..3)
 * @param str       : string to store in internal buffer
 * @param c         : cell to reference in callbacks
 * @param fn_copy   : callback to any paste action
 * @param fn_paste  : callback to ownership loss
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate to the internal buffer.
 * @error DG_CORE_ERRNO_XCB    : failed to obtain clipboard ownership
 */
void dg_core_clipboard_copy(int clipboard, const char *str, dg_core_cell_t *c,
	                        void (*fn_copy)(int clipboard, dg_core_cell_t *c),
	                        void (*fn_lost)(int clipboard, dg_core_cell_t *c));

/**
 * Delete and free the contents of the given clipboard (if owned) and renounce ownership.
 * If any data is held by any clipboard when the session is reseted with dg_core_reset(), it will be freed
 * automatically.
 * 
 * @param clipboard : target clipboard (1..3)
 *
 * @error DG_CORE_ERRNO_XCB : failed to renounce selection ownership
 */
void dg_core_clipboard_delete(int clipboard);

/**
 * Returns the contents of a given clipboard as a string char array (null terminated). The array is malloc'd
 * internally and therefore needs to be explicitely freed.
 *
 * @param clipboard : target clipboard (1..3)
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory to allocate to the returned string
 * @error DG_CORE_ERRNO_XCB    : failed to obtain clipboard content
 */
char *dg_core_clipboard_paste(int clipboard);

/************************************************************************************************************/
/* POPUP ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Creates a popup holding the given cell with the given geometry and position (in pixels). In case the
 * created popup does not fits on the monitor on which the coordinates px and py are, its width or height will
 * be inverted and px_alt / py_alt will be used starting coordinates instead.
 * However, the given geometry and positions values are only suggestions, as the end user, through the config,
 * can override theses values to have the popup spawn at a fixed position and size (like on mobile
 * applications).
 * If a w_ref window is given, all position coordinates px, py, px_alt and py_alt are interpreted relative to
 * the given window positon. Otherwhise, the coorinates are relative to the screen.
 *
 * @param c      : cell that makes the body of the popup
 * @param w_ref  : window to use as cooridinate reference
 * @param px     : suggested spawn point of the popup (left side)
 * @param px_alt : suggested alternative spawn point of the popup (right side)
 * @param py     : suggested spawn point of the popup (top side)
 * @param py_alt : suggested alternative spawn point of the popup (bottom side)
 * @param pw     : suggested width of the popup
 * @param ph     : suggested height of the popup
 *
 * @error DG_CORE_ERRNO_MEMORY     : out of memory to allocate to the popup
 * @error DG_CORE_ERRNO_STACK      : failed to push the popup's internal window and grid to the trackers
 * @error DG_CORE_ERRNO_CAIRO_CRIT : failed to create a cairo context
 * @error DG_CORE_ERRNO_XCB_CRIT   : failed to setup necessary X components for a working window
 * @error DG_CORE_ERRNO_XCB        : failed to set some window properties but the popup's window is still
 *                                   created
 */
void dg_core_popup_spawn(dg_core_cell_t *c, dg_core_window_t *w_ref, int16_t px, int16_t px_alt, int16_t py,
                         int16_t py_alt, int16_t pw, int16_t ph);

/**
 * Kill any child popup a given window's popup may have.
 * Useful in popup's cell events (which gives the parent window).
 * This function has no effect on non popup windows created with dg_core_window_create().
 *
 * @param : target popup window
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the window from the window tracker
 * @error DG_CORE_ERRNO_XCB   : some internal X components may have not been properly freed
 */
void dg_core_popup_kill_children(dg_core_window_t *w);

/**
 * Kill all active popups and give back control to the last focused window.
 * If there were no active popups, this function has no effect.
 *
 * @error DG_CORE_ERRNO_STACK : failed to pull the window from the window tracker
 * @error DG_CORE_ERRNO_XCB   : some internal X components may have not been properly freed
 */
void dg_core_popup_kill_all(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_H */
