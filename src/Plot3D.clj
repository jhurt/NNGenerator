;;Copyright (c) 2009, University of Nevada, Las Vegas
;;All rights reserved.
;;
;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;    * Neither the name of the University of Nevada, Las Vegas, nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;use Java OpenGL wrapper, jogl, to display data on a 3D coordinate system
(import
  '(java.awt Frame Dimension)
  '(javax.swing JFrame SwingUtilities)
  '(java.awt.event MouseMotionAdapter MouseEvent MouseAdapter MouseWheelListener MouseWheelEvent)
  '(javax.media.opengl GLCanvas GLEventListener GL GLAutoDrawable)
  '(javax.media.opengl.glu GLU)
  '(com.sun.opengl.util Animator))

(defstruct Point :x :y)
(defstruct Vertex :x :y :z)

(def frame (new JFrame "3D Plot"))
(def glu (new GLU))

(def lastDragPoint (ref nil))
(def currentScale (ref 1.0))

(def rotationDegreesX (ref 0.0))
(def rotationDegreesY (ref 0.0))
(def rotationDegreesZ (ref 0.0))

(def canvas (new GLCanvas))

(defn getRandomVertex []
  (let [x (. Math ceil (* 50 (. Math random)))
        z (. Math ceil (* 50 (. Math random)))
        y (* 5 (+ (. Math cos (* x 0.2)) (. Math cos (* z 0.2))))]
    (struct Vertex x y z)))

(defn getSomeVertices ([] (cons (getRandomVertex) (getSomeVertices 1)))
  ([x]
    (lazy-seq
      (cons (getRandomVertex) (getSomeVertices 1)))))

(defn drawVertices [#^GL gl vertices]
  (if (seq vertices)
    (let [vertex (first vertices)]
      (if (not (nil? vertex))
        (.glVertex3f gl (vertex :x) (vertex :y) (vertex :z)))
      (drawVertices gl (rest vertices)))))

(defn drawAllVertices [#^GL gl vertices]
  (doto gl
    (.glPointSize 3.0)
    (.glColor3d 0.0 0.0 1.0)
    (.glBegin GL/GL_POINTS))
  (drawVertices gl vertices)
  (.glEnd gl))

(defn drawAxes [#^GL gl max]
  (doto gl
    (.glBegin GL/GL_LINES)
    (.glColor3d 1.0 0.0 0.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d max 0.0 0.0)
    (.glColor3d 0.0 1.0 0.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d 0.0 max 0.0)
    (.glColor3d 1.0 0.0 1.0)
    (.glVertex3d 0.0 0.0 0.0)
    (.glVertex3d 0.0 0.0 max)
    (.glEnd)))

(defn rotateMatrix [#^GL gl]
  (if (not= 0.0 @rotationDegreesX) (.glRotated gl @rotationDegreesX 1.0 0.0 0.0))
  (if (not= 0.0 @rotationDegreesY) (.glRotated gl @rotationDegreesY 0.0 1.0 0.0))
  (if (not= 0.0 @rotationDegreesZ) (.glRotated gl @rotationDegreesZ 0.0 0.0 1.0)))

(defn scaleMatrix [#^GL gl]
  (if (not= 1.0 @currentScale) (.glScaled gl @currentScale @currentScale @currentScale)))

(def canvasEventHandler (proxy [GLEventListener] []
  (init [#^GLAutoDrawable drawable]
    (let [gl (.getGL drawable)]
      (doto gl
        (.glLoadIdentity)
        (.glShadeModel GL/GL_SMOOTH)
        (.glEnable GL/GL_DEPTH_TEST)
        (.glHint GL/GL_PERSPECTIVE_CORRECTION_HINT GL/GL_NICEST))))

  (display [#^GLAutoDrawable drawable]
    (let [gl (.getGL drawable)
          vertices (take 5000 (getSomeVertices))]
      (doto gl
        (.glClear (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))
        (.glMatrixMode GL/GL_MODELVIEW)
        (.glLoadIdentity))
      (scaleMatrix gl)
      (rotateMatrix gl)
      (drawAxes gl 100.0)
      (drawAllVertices gl vertices)
      (.glFlush gl)))

  (reshape
    [#^GLAutoDrawable drawable x y w h]
    (let [gl (.getGL drawable)
          aspect (/ (double w) (double h))]))
  (displayChanged [#^GLAutoDrawable drawable m d])))

(def mouseMotionHandler (proxy [MouseMotionAdapter] []
  ;Fired once before a sequence of 0 or more mouse drag events
  (mouseMoved [#^MouseEvent event]
    (dosync (ref-set lastDragPoint (struct Point (.getX event) (.getY event)))))
  ;Fired many times during a mouse drag
  (mouseDragged [#^MouseEvent event]
    (if (not (nil? lastDragPoint))
      (let [deltaX (- (.getX event) (@lastDragPoint :x))
            deltaY (- (.getY event) (@lastDragPoint :y))]
        (dosync
          (ref-set lastDragPoint (struct Point (.getX event) (.getY event)))
          (alter rotationDegreesY (fn [x] (+ x (/ deltaX 2.0))))
          (alter rotationDegreesZ (fn [x] (+ x (* -1.0 (/ deltaY 4.0)))))
          (alter rotationDegreesX (fn [x] (+ x (/ deltaY 4.0)))))
        (.display canvas))))))

(def mouseHandler (proxy [MouseAdapter] []
  (mouseClicked [#^MouseEvent event]
    (let [clickCount (.getClickCount event)]
      (if (= 2 clickCount)
        (dosync
          (ref-set currentScale 1.0)
          (ref-set rotationDegreesX 0.0) (ref-set rotationDegreesY 0.0) (ref-set rotationDegreesZ 0.0))
          (.display canvas))))))

(def mouseWheelHandler (proxy [MouseWheelListener] []
  (mouseWheelMoved [#^MouseWheelEvent event]
    (let [wheelRotation (.getWheelRotation event)
          scaleAdjustment (if (< 0 wheelRotation) 0.75 1.33333334)]
      (dosync (ref-set currentScale (* @currentScale scaleAdjustment)))
      (.display canvas)))))

(defn main []
  (let []
    (.addMouseListener canvas mouseHandler)
    (.addMouseMotionListener canvas mouseMotionHandler)
    (.addMouseWheelListener canvas mouseWheelHandler)
    (.addGLEventListener canvas canvasEventHandler)
    (.addMouse
      (. canvas (setPreferredSize (new Dimension 400 300)))
      (.. frame (getContentPane) (add canvas))
      (doto frame
        (.setSize 400 300)
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
        (.pack)
        (.setVisible true))
      (.requestFocus canvas))))
