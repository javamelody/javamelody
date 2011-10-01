package net.bull.javamelody.swing.util;

import java.awt.AWTException;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Window;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JApplet;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JRootPane;
import javax.swing.JWindow;
import javax.swing.LookAndFeel;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.SwingUtilities;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;

/**
 * The JGoodies Looks implementation of <code>PopupFactory</code>.
 * Adds a drop shadow border to all popups except ComboBox popups.
 *
 * @author JGoodies Karsten Lentzsch
 * @author Andrej Golovnin
 * @author Karsten Lentzsch
 * @version 1.2
 *
 * @see java.awt.AWTPermission
 * @see java.awt.Robot
 * @see javax.swing.Popup
 * @see LookAndFeel#initialize
 * @see LookAndFeel#uninitialize
 */
public final class ShadowPopupFactory extends PopupFactory {

	/**
	 * In the case of heavy weight popups, snapshots of the screen background
	 * will be stored as client properties of the popup contents' parent.
	 * These snapshots will be used by the popup border to simulate the drop
	 * shadow effect. The two following constants define the names of
	 * these client properties.
	 */
	static final String PROP_HORIZONTAL_BACKGROUND = "hShadowBg";
	static final String PROP_VERTICAL_BACKGROUND = "vShadowBg";

	/**
	 * The 'scratch pad' objects used to calculate dirty regions of
	 * the screen snapshots.
	 */
	static final Point POINT = new Point();
	static final Rectangle RECT = new Rectangle();

	/**
	 * The PopupFactory used before this PopupFactory has been installed
	 * in <code>#install</code>. Used to restored the original state
	 * in <code>#uninstall</code>.
	 */
	private final PopupFactory storedFactory;

	// Instance Creation ******************************************************

	private ShadowPopupFactory(PopupFactory storedFactory) {
		super();
		this.storedFactory = storedFactory;
	}

	// API ********************************************************************

	/**
	 * Installs the ShadowPopupFactory as the shared popup factory
	 * on non-Mac platforms. Also stores the previously set factory,
	 * so that it can be restored in <code>#uninstall</code>.<p>
	 *
	 * In some Mac Java environments the popup factory throws
	 * a NullPointerException when we call <code>#getPopup</code>.<p>
	 *
	 * The Mac case shows that we may have problems replacing
	 * non PopupFactory instances. Therefore we should consider
	 * replacing only instances of PopupFactory.
	 *
	 * @see #uninstall()
	 */
	public static void install() {
		final String os = System.getProperty("os.name");
		final boolean macintosh = os != null && os.indexOf("Mac") != -1;
		if (macintosh) {
			return;
		}

		final PopupFactory factory = PopupFactory.getSharedInstance();
		if (factory instanceof ShadowPopupFactory) {
			return;
		}

		PopupFactory.setSharedInstance(new ShadowPopupFactory(factory));
	}

	/**
	 * Uninstalls the ShadowPopupFactory and restores the original
	 * popup factory as the new shared popup factory.
	 *
	 * @see #install()
	 */
	public static void uninstall() {
		final PopupFactory factory = PopupFactory.getSharedInstance();
		if (!(factory instanceof ShadowPopupFactory)) {
			return;
		}

		final PopupFactory stored = ((ShadowPopupFactory) factory).storedFactory;
		PopupFactory.setSharedInstance(stored);
	}

	/**
	 * Creates a <code>Popup</code> for the Component <code>owner</code>
	 * containing the Component <code>contents</code>. In addition to
	 * the superclass behavior, we try to return a Popup that has a drop shadow,
	 * if popup drop shadows are active - as returned by
	 * <code>Options#isPopupDropShadowActive</code>.<p>
	 *
	 * <code>owner</code> is used to determine which <code>Window</code> the new
	 * <code>Popup</code> will parent the <code>Component</code> the
	 * <code>Popup</code> creates to. A null <code>owner</code> implies there
	 * is no valid parent. <code>x</code> and
	 * <code>y</code> specify the preferred initial location to place
	 * the <code>Popup</code> at. Based on screen size, or other paramaters,
	 * the <code>Popup</code> may not display at <code>x</code> and
	 * <code>y</code>.<p>
	 *
	 * We invoke the super <code>#getPopup</code>, not the one in the
	 * stored factory, because the popup type is set in this instance,
	 * not in the stored one.
	 *
	 * @param owner    Component mouse coordinates are relative to, may be null
	 * @param contents Contents of the Popup
	 * @param x        Initial x screen coordinate
	 * @param y        Initial y screen coordinate
	 * @return Popup containing Contents
	 */
	@Override
	public Popup getPopup(Component owner, Component contents, int x, int y) {
		final Popup popup = super.getPopup(owner, contents, x, y);
		return ShadowPopup.getInstance(owner, contents, x, y, popup);
	}

	/**
	 * Does all the magic for getting popups with drop shadows.
	 * It adds the drop shadow border to the Popup,
	 * in <code>#show</code> it snapshots the screen background as needed,
	 * and in <code>#hide</code> it cleans up all changes made before.
	 *
	 * @author Andrej Golovnin
	 * @version 1.4
	 */
	public static final class ShadowPopup extends Popup {

		/**
		 * Max number of items to store in the cache.
		 */
		private static final int MAX_CACHE_SIZE = 5;

		/**
		 * The cache to use for ShadowPopups.
		 */
		private static List<ShadowPopup> cache;

		/**
		 * The singleton instance used to draw all borders.
		 */
		private static final Border SHADOW_BORDER = ShadowPopupBorder.getInstance();

		/**
		 * The size of the drop shadow.
		 */
		private static final int SHADOW_SIZE = 5;

		/**
		 * The component mouse coordinates are relative to, may be null.
		 */
		private Component owner;

		/**
		 * The contents of the popup.
		 */
		private Component contents;

		/**
		 * The desired x and y location of the popup.
		 */
		private int x, y;

		/**
		 * The real popup. The #show() and #hide() methods will delegate
		 * all calls to these popup.
		 */
		private Popup popup;

		/**
		 * The border of the contents' parent replaced by SHADOW_BORDER.
		 */
		private Border oldBorder;

		/**
		 * The old value of the opaque property of the contents' parent.
		 */
		private boolean oldOpaque;

		/**
		 * The heavy weight container of the popup contents, may be null.
		 */
		private Container heavyWeightContainer;

		// Returns a previously used <code>ShadowPopup</code>, or a new one
		// if none of the popups have been recycled.
		static Popup getInstance(Component owner, Component contents, int x, int y, Popup delegate) {
			ShadowPopup result;
			synchronized (ShadowPopup.class) {
				if (cache == null) {
					cache = new ArrayList<ShadowPopup>(MAX_CACHE_SIZE);
				}
				if (!cache.isEmpty()) {
					result = cache.remove(0);
				} else {
					result = new ShadowPopup();
				}
			}
			result.reset(owner, contents, x, y, delegate);
			return result;
		}

		//Recycles the ShadowPopup.
		private static void recycle(ShadowPopup popup) {
			synchronized (ShadowPopup.class) {
				if (cache.size() < MAX_CACHE_SIZE) {
					cache.add(popup);
				}
			}
		}

		/**
		 * Hides and disposes of the <code>Popup</code>. Once a <code>Popup</code>
		 * has been disposed you should no longer invoke methods on it. A
		 * <code>dispose</code>d <code>Popup</code> may be reclaimed and later used
		 * based on the <code>PopupFactory</code>. As such, if you invoke methods
		 * on a <code>disposed</code> <code>Popup</code>, indeterminate
		 * behavior will result.<p>
		 *
		 * In addition to the superclass behavior, we reset the stored
		 * horizontal and vertical drop shadows - if any.
		 */
		@Override
		public void hide() {
			if (contents == null) {
				return;
			}

			final JComponent parent = (JComponent) contents.getParent();
			popup.hide();
			if (parent != null && parent.getBorder() == SHADOW_BORDER) {
				parent.setBorder(oldBorder);
				parent.setOpaque(oldOpaque);
				oldBorder = null;
				if (heavyWeightContainer != null) {
					parent.putClientProperty(ShadowPopupFactory.PROP_HORIZONTAL_BACKGROUND, null);
					parent.putClientProperty(ShadowPopupFactory.PROP_VERTICAL_BACKGROUND, null);
					heavyWeightContainer = null;
				}
			}
			owner = null;
			contents = null;
			popup = null;
			recycle(this);
		}

		/**
		 * Makes the <code>Popup</code> visible. If the popup has a
		 * heavy-weight container, we try to snapshot the background.
		 * If the <code>Popup</code> is currently visible, it remains visible.
		 */
		@Override
		public void show() {
			if (heavyWeightContainer != null) {
				snapshot();
			}
			popup.show();
		}

		/**
		 * Reinitializes this ShadowPopup using the given parameters.
		 *
		 * @param newOwner component mouse coordinates are relative to, may be null
		 * @param newContents the contents of the popup
		 * @param newX the desired x location of the popup
		 * @param newY the desired y location of the popup
		 * @param newPopup the popup to wrap
		 */
		private void reset(Component newOwner, Component newContents, int newX, int newY,
				Popup newPopup) {
			this.owner = newOwner;
			this.contents = newContents;
			this.popup = newPopup;
			this.x = newX;
			this.y = newY;
			if (newOwner instanceof JComboBox) {
				return;
			}
			// Do not install the shadow border when the contents
			// has a preferred size less than or equal to 0.
			// We can't use the size, because it is(0, 0) for new popups.
			final Dimension contentsPrefSize = newContents.getPreferredSize();
			if (contentsPrefSize.width <= 0 || contentsPrefSize.height <= 0) {
				return;
			}
			for (Container p = newContents.getParent(); p != null; p = p.getParent()) {
				if (p instanceof JWindow || p instanceof Panel) {
					// Workaround for the gray rect problem.
					p.setBackground(newContents.getBackground());
					heavyWeightContainer = p;
					break;
				}
			}
			final JComponent parent = (JComponent) newContents.getParent();
			oldOpaque = parent.isOpaque();
			oldBorder = parent.getBorder();
			parent.setOpaque(false);
			parent.setBorder(SHADOW_BORDER);
			// Pack it because we have changed the border.
			if (heavyWeightContainer != null) {
				heavyWeightContainer.setSize(heavyWeightContainer.getPreferredSize());
			} else {
				parent.setSize(parent.getPreferredSize());
			}
		}

		/**
		 * Snapshots the background. The snapshots are stored as client
		 * properties of the contents' parent. The next time the border is drawn,
		 * this background will be used.<p>
		 *
		 * Uses a robot on the default screen device to capture the screen
		 * region under the drop shadow. Does <em>not</em> use the window's
		 * device, because that may be an outdated device (due to popup reuse)
		 * and the robot's origin seems to be adjusted with the default screen
		 * device.
		 *
		 * @return boolean
		 * @see #show()
		 */
		private boolean snapshot() {
			try {
				final Dimension size = heavyWeightContainer.getPreferredSize();
				final int width = size.width;
				final int height = size.height;

				// Avoid unnecessary and illegal screen captures
				// for degenerated popups.
				if (width <= 0 || height <= SHADOW_SIZE) {
					return false;
				}

				final Robot robot = new Robot(); // uses the default screen device

				RECT.setBounds(x, y + height - SHADOW_SIZE, width, SHADOW_SIZE);
				final BufferedImage hShadowBg = robot.createScreenCapture(RECT);

				RECT.setBounds(x + width - SHADOW_SIZE, y, SHADOW_SIZE, height - SHADOW_SIZE);
				final BufferedImage vShadowBg = robot.createScreenCapture(RECT);

				final JComponent parent = (JComponent) contents.getParent();
				parent.putClientProperty(ShadowPopupFactory.PROP_HORIZONTAL_BACKGROUND, hShadowBg);
				parent.putClientProperty(ShadowPopupFactory.PROP_VERTICAL_BACKGROUND, vShadowBg);

				final Container layeredPane = getLayeredPane();
				if (layeredPane == null) {
					// This could happen if owner is null.
					return false;
				}

				POINT.x = x;
				POINT.y = y;
				SwingUtilities.convertPointFromScreen(POINT, layeredPane);

				// If needed paint dirty region of the horizontal snapshot.
				RECT.x = POINT.x;
				RECT.y = POINT.y + height - SHADOW_SIZE;
				RECT.width = width;
				RECT.height = SHADOW_SIZE;

				paintShadow(hShadowBg, layeredPane);

				// If needed paint dirty region of the vertical snapshot.
				RECT.x = POINT.x + width - SHADOW_SIZE;
				RECT.y = POINT.y;
				RECT.width = SHADOW_SIZE;
				RECT.height = height - SHADOW_SIZE;

				paintShadow(vShadowBg, layeredPane);
			} catch (final AWTException e) {
				return true;
			} catch (final SecurityException e) {
				return true;
			}
			return false;
		}

		/**
		 * If needed paint dirty region of the snapshot
		 *
		 * @param shadowBg
		 *           BufferedImage
		 * @param layeredPane
		 *           Container
		 */
		private void paintShadow(final BufferedImage shadowBg, final Container layeredPane) {
			final int layeredPaneWidth = layeredPane.getWidth();
			final int layeredPaneHeight = layeredPane.getHeight();

			if (RECT.x + RECT.width > layeredPaneWidth) {
				RECT.width = layeredPaneWidth - RECT.x;
			}
			if (RECT.y + RECT.height > layeredPaneHeight) {
				RECT.height = layeredPaneHeight - RECT.y;
			}
			if (!RECT.isEmpty()) {
				final Graphics g = shadowBg.createGraphics();
				g.translate(-RECT.x, -RECT.y);
				g.setClip(RECT);
				if (layeredPane instanceof JComponent) {
					final JComponent c = (JComponent) layeredPane;
					final boolean doubleBuffered = c.isDoubleBuffered();
					c.setDoubleBuffered(false);
					c.paintAll(g);
					c.setDoubleBuffered(doubleBuffered);
				} else {
					layeredPane.paintAll(g);
				}
				g.dispose();
			}
		}

		/**
		 * @return the top level layered pane which contains the owner.
		 */
		private Container getLayeredPane() {
			// The code below is copied from PopupFactory#LightWeightPopup#show()
			Container parent = null;
			if (owner != null) {
				parent = owner instanceof Container ? (Container) owner : owner.getParent();
			}
			// Try to find a JLayeredPane and Window to add
			for (Container p = parent; p != null; p = p.getParent()) {
				if (p instanceof JRootPane) {
					if (p.getParent() instanceof JInternalFrame) {
						continue;
					}
					parent = ((JRootPane) p).getLayeredPane();
					// Continue, so that if there is a higher JRootPane, we'll
					// pick it up.
				} else if (p instanceof Window) {
					if (parent == null) {
						parent = p;
					}
					break;
				} else if (p instanceof JApplet) {
					// Painting code stops at Applets, we don't want
					// to add to a Component above an Applet otherwise
					// you'll never see it painted.
					break;
				}
			}
			return parent;
		}
	}

	/**
	 * A border with a drop shadow intended to be used as the outer border
	 * of popups. Can paint the screen background if used with heavy-weight
	 * popup windows.
	 *
	 * @author Stefan Matthias Aust
	 * @author Karsten Lentzsch
	 * @author Andrej Golovnin
	 * @version 1.2
	 */
	public static class ShadowPopupBorder extends AbstractBorder {

		/**
		 *
		 */
		private static final long serialVersionUID = -8512231832213353638L;

		/**
		 * The drop shadow needs 5 pixels at the bottom and the right hand side.
		 */
		private static final int SHADOW_SIZE = 5;

		/**
		 * The singleton instance used to draw all borders.
		 */
		private static final ShadowPopupBorder INSTANCE = new ShadowPopupBorder();

		/**
		 * The drop shadow is created from a PNG image with 8 bit alpha channel.
		 */
		@SuppressWarnings("all")
		private static final Image SHADOW = new ImageIcon(
				ShadowPopupBorder.class.getResource("/icons/shadow.png")).getImage();

		// Instance Creation *****************************************************

		/**
		 * Returns the singleton instance used to draw all borders.
		 * @return ShadowPopupBorder
		 */
		public static ShadowPopupBorder getInstance() {
			return INSTANCE;
		}

		/**
		 * Paints the border for the specified component with the specified position and size.
		 * @param c the component for which this border is being painted
		 * @param g the paint graphics
		 * @param x the x position of the painted border
		 * @param y the y position of the painted border
		 * @param width the width of the painted border
		 * @param height the height of the painted border
		 */
		@Override
		public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
			// fake drop shadow effect in case of heavy weight popups
			final JComponent popup = (JComponent) c;
			final Image hShadowBg = (Image) popup
					.getClientProperty(ShadowPopupFactory.PROP_HORIZONTAL_BACKGROUND);
			if (hShadowBg != null) {
				g.drawImage(hShadowBg, x, y + height - 5, c);
			}
			final Image vShadowBg = (Image) popup
					.getClientProperty(ShadowPopupFactory.PROP_VERTICAL_BACKGROUND);
			if (vShadowBg != null) {
				g.drawImage(vShadowBg, x + width - 5, y, c);
			}

			// draw drop shadow
			g.drawImage(SHADOW, x + 5, y + height - 5, x + 10, y + height, 0, 6, 5, 11, null, c);
			g.drawImage(SHADOW, x + 10, y + height - 5, x + width - 5, y + height, 5, 6, 6, 11,
					null, c);
			g.drawImage(SHADOW, x + width - 5, y + 5, x + width, y + 10, 6, 0, 11, 5, null, c);
			g.drawImage(SHADOW, x + width - 5, y + 10, x + width, y + height - 5, 6, 5, 11, 6,
					null, c);
			g.drawImage(SHADOW, x + width - 5, y + height - 5, x + width, y + height, 6, 6, 11, 11,
					null, c);
		}

		/**
		 * Returns the insets of the border.
		 * @param c nothing
		 * @return Insets
		 */
		@Override
		public Insets getBorderInsets(Component c) {
			return new Insets(0, 0, SHADOW_SIZE, SHADOW_SIZE);
		}

		/**
		 * Reinitializes the insets parameter with this Border's current Insets.
		 * @param c the component for which this border insets value applies
		 * @param insets the object to be reinitialized
		 * @return the <code>insets</code> object
		 */
		@Override
		public Insets getBorderInsets(Component c, Insets insets) {
			insets.left = 0;
			insets.top = 0;
			insets.right = SHADOW_SIZE;
			insets.bottom = SHADOW_SIZE;
			return insets;
		}
	}
}
