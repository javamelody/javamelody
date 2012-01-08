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
package net.bull.javamelody.swing.table;

import java.awt.Component;
import java.awt.Container;
import java.awt.Event;
import java.awt.Point;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;

/**
 * Composant Table basique servant de base à MListTable.
 *
 * @author Emeric Vernat
 */
public class MBasicTable extends JTable {
	private static final int ADJUST_COLUMN_WIDTHS_MAX_ROWS = 50;

	private static final long serialVersionUID = 1L;

	// Singleton statique pour renderers par défaut des cellules.
	@SuppressWarnings("all")
	private static final Map<Class<?>, TableCellRenderer> DEFAULT_RENDERERS = new HashMap<Class<?>, TableCellRenderer>(
			25);

	@SuppressWarnings("all")
	private static final KeyHandler KEY_HANDLER = new KeyHandler();

	/**
	 * KeyAdapter.
	 *
	 * @author Emeric Vernat
	 */
	private static class KeyHandler extends KeyAdapter {
		/**
		 * Constructeur.
		 */
		KeyHandler() {
			super();
		}

		@Override
		public void keyPressed(final KeyEvent event) {
			if (event.getSource() instanceof MBasicTable) {
				((MBasicTable) event.getSource()).keyPressed(event);
			}
		}
	}

	private static class TableHeader extends JTableHeader {
		private static final long serialVersionUID = 1L;

		TableHeader(TableColumnModel columnModel) {
			super(columnModel);
		}

		@Override
		public String getToolTipText(MouseEvent e) {
			final Point p = e.getPoint();
			final int index = columnModel.getColumnIndexAtX(p.x);
			return String.valueOf(columnModel.getColumn(index).getHeaderValue());
		}
	}

	/**
	 * Constructeur.
	 *
	 * @param dataModel
	 *           Modèle pour les données (par exemple, MTableModel)
	 */
	public MBasicTable(final TableModel dataModel) {
		super(dataModel);
		setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		// setAutoCreateColumnsFromModel(false);
		// par défaut, on laisse AUTO_RESIZE_SUBSEQUENT_COLUMNS
		// setAutoResizeMode(AUTO_RESIZE_OFF);
		// setDragEnabled(true);
		addKeyListener(KEY_HANDLER);
	}

	/**
	 * Adapte les largeurs des colonnes selon les données de cette table. <br/>
	 * Pour chaque colonne la taille préférée est déterminée selon la valeur (et le renderer) du header et selon la valeur (et le renderer) de chaque cellule.
	 */
	public void adjustColumnWidths() {
		if (ADJUST_COLUMN_WIDTHS_MAX_ROWS > 0) {
			TableColumn tableColumn;
			TableCellRenderer renderer;
			Object value;
			Component component;
			final int columnCount = getColumnCount();
			final int rowCount = Math.min(getRowCount(), ADJUST_COLUMN_WIDTHS_MAX_ROWS);
			int columnWidth;
			final int maxWidth = 250; // taille ajustée maximum (15 minimum par défaut)

			// Boucle sur chaque colonne et chaque ligne.
			// Trouve le max de la largeur du header et de chaque cellule
			// et fixe la largeur de la colonne en fonction.
			for (int colNum = 0; colNum < columnCount; colNum++) {
				tableColumn = columnModel.getColumn(colNum);
				if (tableColumn.getMaxWidth() <= 0) {
					continue; // colonne invisible
				}

				renderer = getTableHeader().getDefaultRenderer();
				value = tableColumn.getHeaderValue();
				component = renderer.getTableCellRendererComponent(this, value, false, false, -1,
						colNum);
				columnWidth = component.getPreferredSize().width + 10;
				renderer = getCellRenderer(-1, colNum);

				for (int rowNum = 0; rowNum < rowCount; rowNum++) {
					value = getValueAt(rowNum, colNum);
					component = renderer.getTableCellRendererComponent(this, value, false, false,
							rowNum, colNum);
					columnWidth = Math.max(columnWidth, component.getPreferredSize().width);
				}
				columnWidth = Math.min(maxWidth, columnWidth);

				tableColumn.setPreferredWidth(columnWidth + getIntercellSpacing().width);
			}
		}
	}

	@Override
	protected void configureEnclosingScrollPane() {
		// Si cette table est la viewportView d'un JScrollPane (la situation habituelle),
		// configure ce ScrollPane en positionnant la barre verticale à "always"
		// (et en installant le tableHeader comme columnHeaderView).
		super.configureEnclosingScrollPane();

		final Container parent = getParent();
		if (parent instanceof JViewport && parent.getParent() instanceof JScrollPane) {
			final JScrollPane scrollPane = (JScrollPane) parent.getParent();
			if (scrollPane.getVerticalScrollBar() != null) {
				scrollPane.getVerticalScrollBar().setFocusable(false);
			}
			if (scrollPane.getHorizontalScrollBar() != null) {
				scrollPane.getHorizontalScrollBar().setFocusable(false);
			}

			final JViewport viewport = scrollPane.getViewport();
			if (viewport == null || viewport.getView() != this) {
				return;
			}

			scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void createDefaultRenderers() {
		final Map<Class<?>, TableCellRenderer> map = getDefaultTableCellRenderers();
		super.defaultRenderersByColumnClass = new Hashtable<Class<?>, TableCellRenderer>(map.size());
		super.defaultRenderersByColumnClass.putAll(map);
	}

	@Override
	protected JTableHeader createDefaultTableHeader() {
		return new TableHeader(columnModel);
	}

	/**
	 * Retourne la map par défaut des renderers de cellules avec pour clé le type des valeurs.
	 *
	 * @return Map
	 */
	private Map<Class<?>, TableCellRenderer> getDefaultTableCellRenderers() {
		if (DEFAULT_RENDERERS.isEmpty()) {
			DEFAULT_RENDERERS.put(Boolean.class, new MBooleanTableCellRenderer());
			DEFAULT_RENDERERS.put(Double.class, new MDoubleTableCellRenderer());
			DEFAULT_RENDERERS.put(Float.class, new MDoubleTableCellRenderer());
			DEFAULT_RENDERERS.put(java.math.BigDecimal.class, new MDoubleTableCellRenderer());
			DEFAULT_RENDERERS.put(Integer.class, new MIntegerTableCellRenderer());
			DEFAULT_RENDERERS.put(Long.class, new MIntegerTableCellRenderer());
			DEFAULT_RENDERERS.put(java.math.BigInteger.class, new MIntegerTableCellRenderer());
			DEFAULT_RENDERERS.put(GregorianCalendar.class, new MDateTableCellRenderer());
			DEFAULT_RENDERERS.put(Date.class, new MDateTableCellRenderer());
			DEFAULT_RENDERERS.put(java.sql.Date.class, new MDateTableCellRenderer());
			DEFAULT_RENDERERS.put(java.sql.Timestamp.class, new MDateTableCellRenderer());
			DEFAULT_RENDERERS.put(ArrayList.class, new MListTableCellRenderer()); // NOPMD
			DEFAULT_RENDERERS.put(HashMap.class, new MListTableCellRenderer()); // NOPMD
			DEFAULT_RENDERERS.put(Hashtable.class, new MListTableCellRenderer()); // NOPMD
			DEFAULT_RENDERERS.put(Object.class, new MDefaultTableCellRenderer());
			DEFAULT_RENDERERS.put(String.class, new MDefaultTableCellRenderer());
			DEFAULT_RENDERERS.put(ImageIcon.class, new MImageIconTableCellRenderer());
			DEFAULT_RENDERERS.put(Component.class, new MComponentTableCellRenderer());
		}
		return DEFAULT_RENDERERS;
	}

	/** {@inheritDoc} */
	@Override
	public TableCellRenderer getDefaultRenderer(final Class<?> columnClass) {
		// si c'est une instance de Collection on prend le renderer de ArrayList
		// (car par ex., le résultat de Collections.unmodifiableList est une interface de List
		// sans classe "connue" donc son renderer par défaut n'est pas paramétrable)
		if (columnClass != null && Collection.class.isAssignableFrom(columnClass)) {
			return super.getDefaultRenderer(ArrayList.class); // NOPMD
		}
		return super.getDefaultRenderer(columnClass);
	}

	/** {@inheritDoc} */
	@Override
	public String getToolTipText(final MouseEvent event) {
		final int row = rowAtPoint(event.getPoint());
		final int column = columnAtPoint(event.getPoint());
		if (row != -1 && column != -1) {
			String tip = super.getToolTipText(event);
			if (tip == null) {
				tip = getTextAt(row, column);
				if (tip == null || tip.length() == 0) { // NOPMD
					tip = super.getToolTipText();
				}
			}
			return tip;
		}
		return super.getToolTipText();
	}

	/**
	 * Renvoie le texte affiché à la position demandée.
	 *
	 * @return String
	 * @param row
	 *           int
	 * @param column
	 *           int
	 */
	public String getTextAt(final int row, final int column) {
		final Object value = getValueAt(row, column);

		String text = "";
		if (value != null) {
			final TableCellRenderer renderer = getCellRenderer(row, column);
			final Component rendererComponent = renderer.getTableCellRendererComponent(this, value,
					false, false, row, column);
			if (rendererComponent instanceof JLabel) {
				text = ((JLabel) rendererComponent).getText();
				text = getToolTipTextIfNoText(text, rendererComponent);
			} else if (rendererComponent instanceof JTextComponent) {
				text = ((JTextComponent) rendererComponent).getText();
				text = getToolTipTextIfNoText(text, rendererComponent);
			} else if (rendererComponent instanceof JCheckBox) {
				text = String.valueOf(((JCheckBox) rendererComponent).isSelected() ? "vrai"
						: "faux");
			} else {
				text = value.toString();
			}
		}
		return text;
	}

	private String getToolTipTextIfNoText(final String text, final Component rendererComponent) {
		if (text == null || text.length() == 0) {
			String toolTipText = ((JComponent) rendererComponent).getToolTipText();
			if (toolTipText == null) {
				toolTipText = "";
			}
			return toolTipText;
		}
		return text;
	}

	/**
	 * Gestion des événements clavier sur cette table.
	 *
	 * @param event
	 *           KeyEvent
	 */
	protected void keyPressed(final KeyEvent event) {
		final int keyCode = event.getKeyCode();
		final int modifiers = event.getModifiers();
		if ((modifiers & Event.CTRL_MASK) != 0 && keyCode == KeyEvent.VK_ADD) {
			adjustColumnWidths();
		}
		// else if (modifiers == 0)
		// {
		// final int selectedColumn = getSelectedColumn() != -1 ? getSelectedColumn() : 0;
		// final int selectedRow = getSelectedRow() != -1 ? getSelectedRow() : 0;
		// final int rowCount = getRowCount();
		// if (isCellEditable(selectedRow, selectedColumn) || rowCount == 0)
		// {
		// return;
		// }
		// final String keyChar = String.valueOf(event.getKeyChar());
		// String text;
		// for (int i = selectedRow + 1; i < rowCount; i++)
		// {
		// text = getTextAt(i, selectedColumn);
		// if (text != null && text.regionMatches(true, 0, keyChar, 0, 1))
		// {
		// setRowSelectionInterval(i, i);
		// setColumnSelectionInterval(selectedColumn, selectedColumn);
		// scrollRectToVisible(getCellRect(i, selectedColumn, true));
		// return;
		// }
		// }
		// for (int i = 0; i <= selectedRow; i++)
		// {
		// text = getTextAt(i, selectedColumn);
		// if (text != null && text.regionMatches(true, 0, keyChar, 0, 1))
		// {
		// setRowSelectionInterval(i, i);
		// setColumnSelectionInterval(selectedColumn, selectedColumn);
		// scrollRectToVisible(getCellRect(i, selectedColumn, true));
		// return;
		// }
		// }
		// }
	}
}
