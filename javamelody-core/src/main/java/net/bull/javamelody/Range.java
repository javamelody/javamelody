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

import java.io.Serializable;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;

/**
 * Cette classe représente une période pour les courbes et les statistiques.
 * Elle contient soit une période fixe (jour, semaine, mois, année, tout, à partir de la date du jour),
 * soit une période personnalisée entre une date de début et une date de fin.
 * @author Emeric Vernat
 */
final class Range implements Serializable {
	private static final long serialVersionUID = 4658258882827669495L;

	private static final long MILLISECONDS_PER_DAY = 24L * 60 * 60 * 1000;

	private final Period period;

	private final Date startDate;

	private final Date endDate;

	private Range(Period period, Date startDate, Date endDate) {
		super();
		assert period != null && startDate == null && endDate == null || period == null
				&& startDate != null && endDate != null && startDate.getTime() <= endDate.getTime();
		this.period = period;
		this.startDate = startDate;
		this.endDate = endDate;
	}

	static Range createPeriodRange(Period period) {
		return new Range(period, null, null);
	}

	static Range createCustomRange(Date startDate, Date endDate) {
		return new Range(null, startDate, endDate);
	}

	static Range parse(String value) {
		final int index = value.indexOf('-');
		if (index == -1) {
			return Period.valueOfIgnoreCase(value).getRange();
		}
		// rq: on pourrait essayer aussi des dateFormat alternatifs,
		// par exemple même pattern mais sans les slashs ou juste avec jour et mois
		final DateFormat dateFormat = I18N.createDateFormat();
		Date startDate;
		try {
			startDate = dateFormat.parse(value.substring(0, index));
		} catch (final ParseException e) {
			startDate = new Date();
		}
		final Calendar minimum = Calendar.getInstance();
		minimum.add(Calendar.YEAR, -2);
		if (startDate.getTime() < minimum.getTimeInMillis()) {
			// pour raison de performance, on limite à 2 ans (et non 2000 ans)
			startDate = minimum.getTime();
		}
		if (startDate.getTime() > System.currentTimeMillis()) {
			// pour raison de performance, on limite à aujourd'hui (et non 2000 ans)
			startDate = new Date();
		}

		Date endDate;
		if (index < value.length() - 1) {
			try {
				endDate = dateFormat.parse(value.substring(index + 1));
			} catch (final ParseException e) {
				endDate = new Date();
			}
			if (endDate.getTime() > System.currentTimeMillis()) {
				// pour raison de performance, on limite à aujourd'hui (et non 2000 ans)
				endDate = new Date();
			}
		} else {
			endDate = new Date();
		}
		if (startDate.after(endDate)) {
			endDate = startDate;
		}

		// la date de fin est incluse jusqu'à 23h59m59s
		// (et le formatage de cette date reste donc au même jour)
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(endDate);
		calendar.set(Calendar.HOUR_OF_DAY, 23);
		calendar.set(Calendar.MINUTE, 59);
		calendar.set(Calendar.SECOND, 59);
		final Date includedEndDate = calendar.getTime();

		return Range.createCustomRange(startDate, includedEndDate);
	}

	Period getPeriod() {
		return period;
	}

	Date getStartDate() {
		return startDate;
	}

	Date getEndDate() {
		return endDate;
	}

	String getValue() {
		if (period == null) {
			final DateFormat dateFormat = I18N.createDateFormat();
			return dateFormat.format(startDate) + '-' + dateFormat.format(endDate);
		}
		return period.getCode();
	}

	String getLabel() {
		if (period == null) {
			final DateFormat dateFormat = I18N.createDateFormat();
			return dateFormat.format(startDate) + " - " + dateFormat.format(endDate);
		}
		return period.getLabel();
	}

	int getDurationDays() {
		if (period == null) {
			// attention endDate contient le dernier jour inclus jusqu'à 23h59m59s (cf parse),
			// donc on ajoute 1s pour compter le dernier jour
			return (int) ((endDate.getTime() + 1000 - startDate.getTime()) / MILLISECONDS_PER_DAY);
		}
		return period.getDurationDays();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[period=" + getPeriod() + ", startDate="
				+ getStartDate() + ", endDate=" + getEndDate() + ']';
	}
}
