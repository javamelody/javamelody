/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JToolTip;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalToolTipUI;

import net.bull.javamelody.swing.table.MDefaultTableCellRenderer;
import net.bull.javamelody.swing.table.MIntegerTableCellRenderer;
import net.bull.javamelody.swing.table.MTable;
import net.bull.javamelody.swing.table.MTableScrollPane;

/**
 * Panel des statistiques.
 * @author Emeric Vernat
 */
class StatisticsTablePanel extends MelodyPanel {
	static final Color DARKER_GREEN = Color.GREEN.darker();
	static final Font LABEL_BOLD_FONT = new JLabel().getFont().deriveFont(Font.BOLD);
	static final Font LABEL_PLAIN_FONT = new JLabel().getFont().deriveFont(Font.PLAIN);

	private static final int IMAGE_HEIGHT = 50;
	private static final int IMAGE_WIDTH = 100;

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("all")
	private final CounterRequestAggregation counterRequestAggregation;
	private final Counter counter;
	private final MTable<CounterRequest> table;
	@SuppressWarnings("all")
	private final Map<String, CounterRequest> counterRequestByRequestName = new HashMap<String, CounterRequest>();
	@SuppressWarnings("all")
	private final Map<String, BufferedImage> requestChartByRequestName = new HashMap<String, BufferedImage>();

	private static final class ResponseSizeMeanTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		ResponseSizeMeanTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(final Object value) {
			super.setValue((Integer) value / 1024);
		}
	}

	private class MeanTableCellRenderer extends MIntegerTableCellRenderer {
		private static final long serialVersionUID = 1L;

		MeanTableCellRenderer() {
			super();
		}

		@Override
		public void setValue(Object value) {
			final Integer mean = (Integer) value;
			final CounterRequestAggregation myCounterRequestAggregation = getCounterRequestAggregation();
			if (mean < myCounterRequestAggregation.getWarningThreshold() || mean == 0) {
				// si cette moyenne est < à la moyenne globale + 1 écart-type (paramétrable), c'est bien
				// (si severeThreshold ou warningThreshold sont à 0 et mean à 0, c'est "info" et non "severe")
				setForeground(DARKER_GREEN);
				setFont(LABEL_PLAIN_FONT);
			} else if (mean < myCounterRequestAggregation.getSevereThreshold()) {
				// sinon, si cette moyenne est < à la moyenne globale + 2 écart-types (paramétrable),
				// attention à cette requête qui est plus longue que les autres
				setForeground(Color.ORANGE);
				setFont(LABEL_BOLD_FONT);
			} else {
				// sinon, (cette moyenne est > à la moyenne globale + 2 écart-types),
				// cette requête est très longue par rapport aux autres ;
				// il peut être opportun de l'optimiser si possible
				setForeground(Color.RED);
				setFont(LABEL_BOLD_FONT);
			}
			super.setValue(mean);
		}
	}

	private class DurationPercentageTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		DurationPercentageTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.RIGHT);
		}

		@Override
		public void setValue(final Object value) {
			final CounterRequest globalRequest = getCounterRequestAggregation().getGlobalRequest();
			if (globalRequest.getDurationsSum() == 0) {
				setText("0");
			} else {
				setText(String.valueOf(100 * (Long) value / globalRequest.getDurationsSum()));
			}
		}
	}

	private class CpuPercentageTableCellRenderer extends MDefaultTableCellRenderer {
		private static final long serialVersionUID = 1L;

		CpuPercentageTableCellRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.RIGHT);
		}

		@Override
		public void setValue(final Object value) {
			final CounterRequest globalRequest = getCounterRequestAggregation().getGlobalRequest();
			if (globalRequest.getCpuTimeSum() == 0) {
				setText("0");
			} else {
				setText(String.valueOf(100 * (Long) value / globalRequest.getCpuTimeSum()));
			}
		}
	}

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

	StatisticsTablePanel(RemoteCollector remoteCollector, Counter counter,
			CounterRequestAggregation counterRequestAggregation) {
		super(remoteCollector);

		assert counter != null;
		assert counterRequestAggregation != null;
		this.counter = counter;
		this.counterRequestAggregation = counterRequestAggregation;

		if (CounterRequestDetailPanel.isRequestGraphDisplayed(counter)) {
			this.table = new MTable<CounterRequest>() {
				private static final long serialVersionUID = 1L;

				@Override
				public JToolTip createToolTip() {
					final ImageToolTip imageToolTip = new ImageToolTip();
					imageToolTip.setComponent(this);
					return imageToolTip;
				}
			};
		} else {
			this.table = new MTable<CounterRequest>();
		}

		addColumns();

		final MTableScrollPane<CounterRequest> tableScrollPane = new MTableScrollPane<CounterRequest>(
				table);
		add(tableScrollPane, BorderLayout.CENTER);
	}

	private void addColumns() {
		final String nameColumnHeader;
		if (isJobCounter()) {
			nameColumnHeader = I18N.getString("Job");
		} else if (isErrorCounter()) {
			nameColumnHeader = I18N.getString("Erreur");
		} else {
			nameColumnHeader = I18N.getString("Requete");
		}
		table.addColumn("name", nameColumnHeader);
		// MMultiLineTableCellRenderer n'est pas défini ici pour la colonne "name"
		// car les dimensions de certains scrollPane ne seraient pas corrects

		final MIntegerTableCellRenderer meanCellRenderer = new MeanTableCellRenderer();
		if (counterRequestAggregation.isTimesDisplayed()) {
			table.addColumn("durationsSum", I18N.getString("temps_cumule"));
			table.addColumn("hits", I18N.getString("Hits"));
			table.addColumn("mean", I18N.getString("Temps_moyen"));
			table.addColumn("maximum", I18N.getString("Temps_max"));
			table.addColumn("standardDeviation", I18N.getString("Ecart_type"));
			table.setColumnCellRenderer("durationsSum", new DurationPercentageTableCellRenderer());
			table.setColumnCellRenderer("mean", meanCellRenderer);
		} else {
			table.addColumn("hits", I18N.getString("Hits"));
		}
		if (counterRequestAggregation.isCpuTimesDisplayed()) {
			table.addColumn("cpuTimeSum", I18N.getString("temps_cpu_cumule"));
			table.addColumn("cpuTimeMean", I18N.getString("Temps_cpu_moyen"));
			table.setColumnCellRenderer("cpuTimeSum", new CpuPercentageTableCellRenderer());
			table.setColumnCellRenderer("cpuTimeMean", meanCellRenderer);
		}
		if (!isErrorAndNotJobCounter()) {
			table.addColumn("systemErrorPercentage", I18N.getString("erreur_systeme"));
		}
		if (counterRequestAggregation.isResponseSizeDisplayed()) {
			table.addColumn("responseSizeMean", I18N.getString("Taille_moyenne"));
			table.setColumnCellRenderer("responseSizeMean", new ResponseSizeMeanTableCellRenderer());
		}
		if (counterRequestAggregation.isChildHitsDisplayed()) {
			table.addColumn("childHitsMean",
					I18N.getFormattedString("hits_fils_moyens", counter.getChildCounterName()));
			table.addColumn("childDurationsMean",
					I18N.getFormattedString("temps_fils_moyen", counter.getChildCounterName()));
		}
	}

	BufferedImage getRequestChartByRequestName(String requestName) throws IOException {
		BufferedImage requestChart;
		if (requestChartByRequestName.containsKey(requestName)) {
			requestChart = requestChartByRequestName.get(requestName);
		} else {
			requestChart = null;
			final CounterRequest counterRequest = getCounterRequestByRequestName(requestName);
			if (counterRequest != null) {
				final byte[] imageData = getRemoteCollector().collectJRobin(counterRequest.getId(),
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
			final List<CounterRequest> requests = table.getList();
			for (final CounterRequest request : requests) {
				counterRequestByRequestName.put(request.getName(), request);
			}
		}
		return counterRequestByRequestName.get(requestName);
	}

	void setList(List<CounterRequest> requests) {
		table.setList(requests);
		requestChartByRequestName.clear();
		counterRequestByRequestName.clear();
	}

	MTable<CounterRequest> getTable() {
		return table;
	}

	CounterRequestAggregation getCounterRequestAggregation() {
		return counterRequestAggregation;
	}

	private boolean isErrorCounter() {
		return counter.isErrorCounter();
	}

	private boolean isJobCounter() {
		return counter.isJobCounter();
	}

	private boolean isErrorAndNotJobCounter() {
		return isErrorCounter() && !isJobCounter();
	}
}
