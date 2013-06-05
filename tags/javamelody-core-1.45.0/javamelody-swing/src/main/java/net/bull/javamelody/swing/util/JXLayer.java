package net.bull.javamelody.swing.util;

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;

import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 * @author Alexander Potochkin
 *
 * https://swinghelper.dev.java.net/
 * http://weblogs.java.net/blog/alexfromsun/
 */
public class JXLayer extends JPanel {
	private static final long serialVersionUID = -180838202361369295L;
	// layering related properties
	private final JComponent contentPane;
	private final JComponent glassPane = new JXGlassPane();

	// painting related properties
	private float alpha = 1;
	private transient BufferedImageOp bio;
	private transient BufferedImage tempSrc;
	private transient BufferedImage tempDst;

	private static class JXGlassPane extends JPanel {
		private static final long serialVersionUID = -6311466609547970582L;

		/**
		 * Constructeur.
		 */
		JXGlassPane() {
			super();
			setOpaque(false);
		}

		/** {@inheritDoc} */
		@Override
		public boolean contains(int x, int y) {
			if (getMouseListeners().length == 0 && getMouseMotionListeners().length == 0
					&& getMouseWheelListeners().length == 0 && !isCursorSet()) {
				return false;
			}
			return super.contains(x, y);
		}
	}

	/**
	 * Constructeur.
	 * @param c JComponent
	 */
	public JXLayer(JComponent c) {
		super(null);
		super.addImpl(glassPane, null, 0);
		super.addImpl(c, null, 1);
		this.contentPane = c;
	}

	/** {@inheritDoc} */
	@Override
	public void doLayout() {
		if (contentPane != null) {
			setPreferredSize(contentPane.getPreferredSize());
			contentPane.setLocation(0, 0);
			contentPane.setSize(getWidth(), getHeight());
		}
		if (glassPane != null) {
			glassPane.setLocation(0, 0);
			glassPane.setSize(getWidth(), getHeight());
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean isOptimizedDrawingEnabled() {
		return false;
	}

	/** {@inheritDoc} */
	@Override
	protected void addImpl(Component comp, Object constraints, int index) {
		contentPane.add(comp, constraints, index);
		doLayout();
	}

	/** {@inheritDoc} */
	@Override
	public void remove(Component comp) {
		contentPane.remove(comp);
	}

	/** {@inheritDoc} */
	@Override
	public void removeAll() {
		contentPane.removeAll();
	}

	/** {@inheritDoc} */
	@Override
	public void setLayout(LayoutManager mgr) {
		if (contentPane != null) {
			contentPane.setLayout(mgr);
		}
	}

	/** {@inheritDoc} */
	@Override
	public LayoutManager getLayout() {
		return contentPane.getLayout();
	}

	/** {@inheritDoc} */
	@Override
	public void setPreferredSize(Dimension preferredSize) {
		contentPane.setPreferredSize(preferredSize);
	}

	/** {@inheritDoc} */
	@Override
	public Dimension getPreferredSize() {
		return contentPane.getPreferredSize();
	}

	/** {@inheritDoc} */
	@Override
	public Dimension getMaximumSize() {
		return contentPane.getMaximumSize();
	}

	/** {@inheritDoc} */
	@Override
	public void setMaximumSize(Dimension maximumSize) {
		contentPane.setMaximumSize(maximumSize);
	}

	/** {@inheritDoc} */
	@Override
	public Dimension getMinimumSize() {
		return contentPane.getMinimumSize();
	}

	/** {@inheritDoc} */
	@Override
	public void setMinimumSize(Dimension minimumSize) {
		contentPane.setMinimumSize(minimumSize);
	}

	// painting

	/**
	 * @return BufferedImageOp
	 */
	public BufferedImageOp getBufferedImageOp() {
		return bio;
	}

	/**
	 * @param bufferedImageOp BufferedImageOp
	 */
	public void setBufferedImageOp(BufferedImageOp bufferedImageOp) {
		if (bufferedImageOp instanceof AffineTransformOp) {
			throw new IllegalArgumentException("AffineTransformOp is not supported");
		}
		this.bio = bufferedImageOp;
		repaint();
	}

	/**
	 * @return float
	 */
	public float getAlpha() {
		return alpha;
	}

	/**
	 * @param alpha float
	 */
	public void setAlpha(float alpha) {
		if (alpha < 0 || alpha > 1) {
			throw new IllegalArgumentException();
		}
		this.alpha = alpha;
		repaint();
	}

	/** {@inheritDoc} */
	@Override
	public void paint(Graphics g) {
		if (bio == null && alpha == 1 || !(g instanceof Graphics2D)) {
			super.paint(g);
			return;
		}

		final Graphics2D g2 = (Graphics2D) g.create();

		Rectangle clipBounds = g2.getClipBounds();

		if (clipBounds == null) {
			clipBounds = new Rectangle(getSize());
		}
		if (clipBounds.isEmpty()) {
			return;
		}

		final boolean isConvolveOp = bio instanceof ConvolveOp;

		if (isConvolveOp) {
			final ConvolveOp cop = (ConvolveOp) bio;
			final Kernel kernel = cop.getKernel();
			clipBounds.grow(kernel.getWidth() / 2, kernel.getHeight() / 2);
		}

		createTempImagesIfNecessary(clipBounds);

		final Graphics2D bufg = (Graphics2D) tempSrc.getGraphics();
		bufg.translate(-clipBounds.x, -clipBounds.y);
		bufg.setClip(clipBounds);
		super.paint(bufg);
		bufg.dispose();

		applyFilter(g2, clipBounds, isConvolveOp);
	}

	private void applyFilter(Graphics2D g2, Rectangle clipBounds, boolean isConvolveOp) {
		if (isConvolveOp) {
			tempDst = bio.filter(tempSrc, tempDst);
		} else {
			tempDst = bio.filter(tempSrc, tempSrc);
		}

		if (isOpaque()) {
			g2.clearRect(clipBounds.x, clipBounds.y, clipBounds.width, clipBounds.height);
		}

		final Composite oldComposite = g2.getComposite();
		if (alpha != 1) {
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha));
		}
		g2.drawImage(tempDst, clipBounds.x, clipBounds.y, null);
		g2.setComposite(oldComposite);

		g2.dispose();
	}

	private void createTempImagesIfNecessary(Rectangle clipBounds) {
		if (tempSrc == null || tempSrc.getWidth() != clipBounds.width
				|| tempSrc.getHeight() != clipBounds.height) {
			tempSrc = getGraphicsConfiguration().createCompatibleImage(clipBounds.width,
					clipBounds.height);
			// TYPE_4BYTE_ABGR the only type which works properly on Linux and Solaris ?
			// tempSrc = new BufferedImage(clipBounds.width, clipBounds.height, BufferedImage.TYPE_4BYTE_ABGR);
			if (bio instanceof ConvolveOp) {
				tempDst = getGraphicsConfiguration().createCompatibleImage(clipBounds.width,
						clipBounds.height);
				// idem ?
			}
		}
	}
}
