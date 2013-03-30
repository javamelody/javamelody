/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.JToolTip;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalToolTipUI;

import net.bull.javamelody.swing.table.MTable;

/**
 * Tableau de requêtes avec graphiques en toolTip
 * @author Emeric Vernat
 */
class CounterRequestTable extends MTable<CounterRequest> {
	private static final long serialVersionUID = 1L;

	private static final int IMAGE_HEIGHT = 50;
	private static final int IMAGE_WIDTH = 100;

	@SuppressWarnings("all")
	private final RemoteCollector remoteCollector;

	@SuppressWarnings("all")
	private final Map<String, CounterRequest> counterRequestByRequestName = new HashMap<String, CounterRequest>();
	@SuppressWarnings("all")
	private final Map<String, BufferedImage> requestChartByRequestName = new HashMap<String, BufferedImage>();

	private class ImageToolTip extends JToolTip {
		private static final long serialVersionUID = 1L;

		ImageToolTip() {
			super();
			setUI(new ImageToolTipUI());
		}
	}

	private class ImageToolTipUI extends MetalToolTipUI {
		ImageToolTipUI() {
			super();
			// parce que ImageToolTipUI hérite de MetalToolTipUI et non de SynthToolTipUI
			// on fixe la couleur pour la propriété "ToolTip.background"
			// sinon ce tooltip aura une moche couleur grise à la place d'un jaune beige
			// (quand il n'y pas d'image jrobin)
			UIManager.put("ToolTip.background", UIManager.getColor("info"));
		}

		@Override
		public void paint(Graphics g, JComponent c) {
			try {
				final String tipText = ((JToolTip) c).getTipText();
				final BufferedImage image = getRequestChartByRequestName(tipText);
				// on affiche que l'image et pas le text dans le tooltip
				//		    FontMetrics metrics = c.getFontMetrics(g.getFont());
				//		    g.setColor(c.getForeground());
				//		    g.drawString(tipText, 1, 1);
				if (image != null) {
					g.drawImage(image, 0, 0, c);
				} else {
					super.paint(g, c);
				}
			} catch (final IOException e) {
				// s'il y a une erreur dans la récupération de l'image tant pis
				super.paint(g, c);
			}
		}

		@Override
		public Dimension getPreferredSize(JComponent c) {
			try {
				final String tipText = ((JToolTip) c).getTipText();
				final BufferedImage image = getRequestChartByRequestName(tipText);
				if (image != null) {
					// pas besoin de calculer la dimension de l'image, puisqu'elle est fixée avant d'avoir l'image
					return new Dimension(image.getWidth(), image.getHeight());
				}
				return super.getPreferredSize(c);
			} catch (final IOException e) {
				// s'il y a une erreur dans la récupération de l'image tant pis
				return super.getPreferredSize(c);
			}

			//			FontMetrics metrics = c.getFontMetrics(c.getFont());
			//		    String tipText = ((JToolTip) c).getTipText();
			//		    BufferedImage image = getRequestChartByRequestName(tipText);
			//		    if (tipText == null) {
			//		      tipText = "";
			//		    }
			//		    int width = SwingUtilities.computeStringWidth(metrics, tipText);
			//		    int height = metrics.getHeight() + image.getHeight(c);
			//
			//		    if (width < image.getWidth(c)) {
			//		      width = image.getWidth(c);
			//		    }
			//		    return new Dimension(width, height);
		}
	}

	CounterRequestTable(RemoteCollector remoteCollector) {
		super();
		this.remoteCollector = remoteCollector;
	}

	static boolean isRequestGraphDisplayed(Counter parentCounter) {
		return !(parentCounter.isErrorCounter() && !parentCounter.isJobCounter())
				&& !parentCounter.isJspOrStrutsCounter();
	}

	@Override
	public JToolTip createToolTip() {
		final ImageToolTip imageToolTip = new ImageToolTip();
		imageToolTip.setComponent(this);
		return imageToolTip;
	}

	@Override
	public void setList(List<CounterRequest> requests) {
		super.setList(requests);
		requestChartByRequestName.clear();
		counterRequestByRequestName.clear();
	}

	BufferedImage getRequestChartByRequestName(String requestName) throws IOException {
		BufferedImage requestChart;
		if (requestChartByRequestName.containsKey(requestName)) {
			requestChart = requestChartByRequestName.get(requestName);
		} else {
			requestChart = null;
			final CounterRequest counterRequest = getCounterRequestByRequestName(requestName);
			if (counterRequest != null) {
				final byte[] imageData = remoteCollector.collectJRobin(counterRequest.getId(),
						IMAGE_WIDTH, IMAGE_HEIGHT);
				if (imageData != null) {
					requestChart = ImageIO.read(new ByteArrayInputStream(imageData));
					// requestChart sera mis dans la map par le put ci-dessous
				}
			}
			// si on n'a pas trouvé la requête dans la liste des requêtes
			// ou si on n'arrive pas à charger l'image depuis le serveur
			// alors il y aura null mis comme image dans la map
			// et au moins on ne cherchera plus inutilement dans cette liste
			// ou on n'essayera plus à chaque fois de charger l'image depuis le serveur
			// et sinon il y aura une image dans la map si tout va bien
			requestChartByRequestName.put(requestName, requestChart);
		}
		return requestChart;
	}

	private CounterRequest getCounterRequestByRequestName(String requestName) {
		// si cela n'a pas déjà été fait précédemment, on met tous les résultats de cette méthode dans une map indexée
		// pour éviter de créer une ArrayList et de reparcourir toutes les requêtes à chaque fois
		if (counterRequestByRequestName.isEmpty()) {
			final List<CounterRequest> requests = getList();
			for (final CounterRequest request : requests) {
				counterRequestByRequestName.put(request.getName(), request);
			}
		}
		return counterRequestByRequestName.get(requestName);
	}
}
