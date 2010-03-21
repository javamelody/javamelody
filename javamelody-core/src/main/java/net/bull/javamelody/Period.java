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

import java.util.Locale;

/**
 * Énumération des périodes possibles.
 * @author Emeric Vernat
 */
enum Period {
	/** Jour. */
	JOUR(1, "calendar_view_day.png", "day"),
	/** Semaine. */
	SEMAINE(7, "calendar_view_week.png", "week"),
	/** Mois. */
	MOIS(31, "calendar_view_month.png", "month"),
	/** Année. */
	ANNEE(366, "calendar.png", "year"),
	/** Tout.
	 * (affiche les graphs sur 2 ans et toutes les requêtes y compris les dernières minutes) */
	TOUT(2 * 366, "calendar.png", "all");

	private final String code;
	private final String mailCode;
	private final int durationDays;
	private final int durationSeconds;
	private final String iconName;
	private final Range range;

	private Period(int durationDays, String iconName, String mailCode) {
		this.durationDays = durationDays;
		this.durationSeconds = durationDays * 24 * 60 * 60;
		this.iconName = iconName;
		this.mailCode = mailCode;
		this.code = this.toString().toLowerCase(Locale.getDefault());
		this.range = Range.createPeriodRange(this);
	}

	static Period valueOfIgnoreCase(String period) {
		return valueOf(period.toUpperCase(Locale.getDefault()).trim());
	}

	static Period valueOfByMailCode(String mailPeriod) {
		final String mailCode = mailPeriod.toLowerCase(Locale.getDefault()).trim();
		for (final Period period : values()) {
			if (period.mailCode.equals(mailCode)) {
				return period;
			}
		}
		throw new IllegalArgumentException(mailPeriod);
	}

	String getCode() {
		return code;
	}

	String getMailCode() {
		return mailCode;
	}

	String getLabel() {
		return I18N.getString(code + "_label");
	}

	String getLinkLabel() {
		return I18N.getString(code + "_link_label");
	}

	int getDurationDays() {
		return durationDays;
	}

	int getDurationSeconds() {
		return durationSeconds;
	}

	String getIconName() {
		return iconName;
	}

	Range getRange() {
		return range;
	}
}
