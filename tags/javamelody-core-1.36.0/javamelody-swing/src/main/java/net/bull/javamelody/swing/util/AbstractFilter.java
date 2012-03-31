package net.bull.javamelody.swing.util;

import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;

/**
 * <p>Provides an abstract implementation of the <code>BufferedImageOp</code>
 * interface. This class can be used to created new image filters based
 * on <code>BufferedImageOp</code>.</p>
 *
 * @author Romain Guy, romain.guy@mac.com
 */
public abstract class AbstractFilter implements BufferedImageOp {
	/**
	 * {@inheritDoc}
	 */
	@Override
	public abstract BufferedImage filter(BufferedImage src, BufferedImage dest);

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Rectangle2D getBounds2D(BufferedImage src) {
		return new Rectangle(0, 0, src.getWidth(), src.getHeight());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BufferedImage createCompatibleDestImage(BufferedImage src, ColorModel destCM) {
		final ColorModel cm;
		if (destCM == null) {
			cm = src.getColorModel();
		} else {
			cm = destCM;
		}

		return new BufferedImage(cm, cm.createCompatibleWritableRaster(src.getWidth(),
				src.getHeight()), cm.isAlphaPremultiplied(), null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Point2D getPoint2D(Point2D srcPt, Point2D dstPt) {
		return (Point2D) srcPt.clone();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public RenderingHints getRenderingHints() { // NOPMD
		return null;
	}
}
