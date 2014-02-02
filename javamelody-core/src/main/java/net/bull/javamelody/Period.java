/*
 * Copyright 2008-2014 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
