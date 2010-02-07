;;Copyright (c) 2010, University of Nevada, Las Vegas
;;All rights reserved.
;;
;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;    * Neither the name of the University of Nevada, Las Vegas, nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns com.jhurt.SwingUtils)

(import '(javax.swing SwingUtilities)
        '(java.awt Dimension DisplayMode Graphics
          GraphicsDevice GraphicsEnvironment))

(defn doOnEdt
"Schedule the action on the AWT event dispatch thread and return immediately after scheduling"
  [#^Runnable action]
  (SwingUtilities/invokeLater #^Runnable action))

(defn doOnEdtAndWait
  "Schedule the action on the AWT event dispatch thread and wait for the action to finish"
  [action]
  (if (SwingUtilities/isEventDispatchThread) (action) (SwingUtilities/invokeAndWait action)))

(defn setSizeBasedOnResolution
  "Set the size of the Swing component based on the resolution of the
  primary display. Defaults to 800x600 if there is a problem
  grabbing the display"
  [component]
	(let [graphicsEnv (GraphicsEnvironment/getLocalGraphicsEnvironment)
				graphicsDevices (. graphicsEnv getScreenDevices)]
			(if (> (alength graphicsDevices) 0)
				(let [displayMode (. (aget graphicsDevices 0) getDisplayMode)]
					(. component setSize (new Dimension
            (- (. displayMode getWidth) 60)
            (- (. displayMode getHeight) 60))))
				(. component  setSize (new Dimension 800 600)))))
