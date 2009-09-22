/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.util.Locale;

/**
 * Énumération des périodes possibles.
 * @author Emeric Vernat
 */
enum Period {
	/** Jour. */
	JOUR(24 * 60 * 60, "calendar_view_day.png"),
	/** Semaine. */
	SEMAINE(7 * 24 * 60 * 60, "calendar_view_week.png"),
	/** Mois. */
	MOIS(31 * 24 * 60 * 60, "calendar_view_month.png"),
	/** Année. */
	ANNEE(366 * 24 * 60 * 60, "calendar.png"),
	/** Tout.
	 * (affiche les graphs sur 2 ans et toutes les requêtes y compris les dernières minutes) */
	TOUT(2 * 366 * 24 * 60 * 60, "calendar.png");

	private final String code;
	private final int durationSeconds;
	private final String iconName;

	private Period(int durationSeconds, String iconName) {
		this.durationSeconds = durationSeconds;
		this.code = this.toString().toLowerCase(Locale.getDefault());
		this.iconName = iconName;
	}

	static Period valueOfIgnoreCase(String period) {
		return valueOf(period.toUpperCase(Locale.getDefault()).trim());
	}

	String getCode() {
		return code;
	}

	String getLabel() {
		return I18N.getString(code + "_label");
	}

	String getLinkLabel() {
		return I18N.getString(code + "_link_label");
	}

	int getDurationSeconds() {
		return durationSeconds;
	}

	String getIconName() {
		return iconName;
	}
}
