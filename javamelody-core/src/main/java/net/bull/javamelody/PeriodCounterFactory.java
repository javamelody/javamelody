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

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * Factory pour les compteurs par jour, par semaine, par mois et par année.
 * @author Emeric Vernat
 */
class PeriodCounterFactory {
	// Note d'implémentation : Calendar.getInstance() crée à chaque appel une nouvelle instance
	// de Calendar à la date et à l'heure courante (cette date-heure peut être modifiée)

	private final Counter currentDayCounter;

	PeriodCounterFactory(Counter currentDayCounter) {
		super();
		assert currentDayCounter != null;
		this.currentDayCounter = currentDayCounter;
	}

	Counter buildNewDayCounter() throws IOException {
		final Calendar start = Calendar.getInstance();
		start.setTime(currentDayCounter.getStartDate());
		if (start.get(Calendar.MONTH) != Calendar.getInstance().get(Calendar.MONTH)) {
			// le mois a changé, on crée un compteur vide qui sera enregistré dans un nouveau fichier;
			// ce compteur agrégé pour le mois est utilisé pour de meilleurs performances sur le compteur de l'année
			// on calcule le monthCounter et on l'enregistre (optimisation pour getYearCounter)
			getMonthCounterAtDate(currentDayCounter.getStartDate());
		}

		return createDayCounterAtDate(new Date());
	}

	// compteur d'un jour donné
	private Counter getDayCounterAtDate(Date day) {
		final Counter dayCounter = createDayCounterAtDate(day);
		try {
			dayCounter.readFromFile();
		} catch (final IOException e) {
			// lecture échouée, tant pis
			// (on n'interrompt pas tout un rapport juste pour un des fichiers illisible)
			LOG.info("read of a counter file failed: " + dayCounter.getName(), e);
		}
		return dayCounter;
	}

	// compteur custom
	Counter getCustomCounter(Range range) {
		assert range.getPeriod() == null;
		final Counter customCounter = createPeriodCounter("yyyy-MM-dd", range.getStartDate());
		addRequestsAndErrorsForRange(customCounter, range);
		return customCounter;
	}

	// compteur du jour courant
	Counter getDayCounter() {
		return currentDayCounter;
	}

	// compteur des 7 derniers jours
	Counter getWeekCounter() {
		final Counter weekCounter = createPeriodCounter("yyyyWW", currentDayCounter.getStartDate());
		addRequestsAndErrorsForRange(weekCounter, Period.SEMAINE.getRange());
		return weekCounter;
	}

	// compteur des 31 derniers jours,
	// ici c'est un mois flottant (ie une durée), et pas un mois entier
	Counter getMonthCounter() {
		final Counter monthCounter = createMonthCounterAtDate(currentDayCounter.getStartDate());
		addRequestsAndErrorsForRange(monthCounter, Period.MOIS.getRange());
		return monthCounter;
	}

	private void addRequestsAndErrorsForRange(Counter counter, Range range) {
		final Calendar dayCalendar = Calendar.getInstance();
		if (range.getPeriod() == null) {
			counter.addRequestsAndErrors(getDayCounterAtDate(range.getEndDate()));
			dayCalendar.setTime(range.getEndDate());
			// issue 122: attention endDate contient un jour jusqu'à 23h59m59s selon Range.parse
			dayCalendar.set(Calendar.HOUR_OF_DAY, 0);
			dayCalendar.set(Calendar.MINUTE, 0);
			dayCalendar.set(Calendar.SECOND, 0);
		} else {
			counter.addRequestsAndErrors(currentDayCounter);
			dayCalendar.setTime(currentDayCounter.getStartDate());
		}
		final int durationDays = range.getDurationDays();
		for (int i = 1; i < durationDays; i++) {
			// TODO optimisation avec getMonthCounterAtDate comme getYearCounter() ?
			dayCalendar.add(Calendar.DAY_OF_YEAR, -1);
			counter.addRequestsAndErrors(getDayCounterAtDate(dayCalendar.getTime()));
		}
		counter.setStartDate(dayCalendar.getTime());
	}

	// compteur des 366 derniers jours
	Counter getYearCounter() throws IOException {
		final Counter yearCounter = createPeriodCounter("yyyy", currentDayCounter.getStartDate());
		yearCounter.addRequestsAndErrors(currentDayCounter);
		final Calendar dayCalendar = Calendar.getInstance();
		final int currentMonth = dayCalendar.get(Calendar.MONTH);
		dayCalendar.setTime(currentDayCounter.getStartDate());
		dayCalendar.add(Calendar.DAY_OF_YEAR, -Period.ANNEE.getDurationDays() + 1);
		yearCounter.setStartDate(dayCalendar.getTime());
		for (int i = 1; i < Period.ANNEE.getDurationDays(); i++) {
			if (dayCalendar.get(Calendar.DAY_OF_MONTH) == 1
					&& dayCalendar.get(Calendar.MONTH) != currentMonth) {
				// optimisation : on récupère les statistiques précédemment calculées pour ce mois entier
				// au lieu de parcourir à chaque fois les statistiques de chaque jour du mois
				yearCounter.addRequestsAndErrors(getMonthCounterAtDate(dayCalendar.getTime()));
				final int nbDaysInMonth = dayCalendar.getActualMaximum(Calendar.DAY_OF_MONTH);
				// nbDaysInMonth - 1 puisque l'itération va ajouter 1 à i et à dayCalendar
				dayCalendar.add(Calendar.DAY_OF_YEAR, nbDaysInMonth - 1);
				i += nbDaysInMonth - 1;
			} else {
				yearCounter.addRequestsAndErrors(getDayCounterAtDate(dayCalendar.getTime()));
			}
			dayCalendar.add(Calendar.DAY_OF_YEAR, 1);
		}
		return yearCounter;
	}

	private Counter getMonthCounterAtDate(Date day) throws IOException {
		final Counter monthCounter = createMonthCounterAtDate(day);
		try {
			final Counter readCounter = new CounterStorage(monthCounter).readFromFile();
			if (readCounter != null) {
				// monthCounter déjà calculé et enregistré
				return readCounter;
			}
		} catch (final IOException e) {
			// lecture échouée, tant pis
			// (on n'interrompt pas tout un rapport juste pour un des fichiers illisible)
			LOG.info("read of a counter file failed: " + monthCounter.getName(), e);
		}
		// monthCounter n'est pas encore calculé (il est calculé à la fin de chaque mois,
		// mais le serveur a pu aussi être arrêté ce jour là),
		// alors on le calcule et on l'enregistre (optimisation pour getYearCounter)
		final Calendar dayCalendar = Calendar.getInstance();
		dayCalendar.setTime(day);
		final int nbDaysInMonth = dayCalendar.getActualMaximum(Calendar.DAY_OF_MONTH);
		for (int i = 1; i <= nbDaysInMonth; i++) {
			dayCalendar.set(Calendar.DAY_OF_MONTH, i);
			monthCounter.addRequestsAndErrors(getDayCounterAtDate(dayCalendar.getTime()));
		}
		monthCounter.writeToFile();
		return monthCounter;
	}

	Counter createDayCounterAtDate(Date day) {
		// le nom du compteur par jour est celui du compteur initial
		// auquel on ajoute la date en suffixe pour que son enregistrement soit unique
		return createPeriodCounter("yyyyMMdd", day);
	}

	private Counter createMonthCounterAtDate(Date day) {
		// le nom du compteur par mois est celui du compteur initial
		// auquel on ajoute le mois en suffixe pour que son enregistrement soit unique
		return createPeriodCounter("yyyyMM", day);
	}

	private Counter createPeriodCounter(String dateFormatPattern, Date date) {
		final String storageName = currentDayCounter.getName() + '_'
				+ new SimpleDateFormat(dateFormatPattern, Locale.getDefault()).format(date);
		// ceci crée une nouvelle instance sans requêtes avec startDate à la date courante
		final Counter result = new Counter(currentDayCounter.getName(), storageName,
				currentDayCounter.getIconName(), currentDayCounter.getChildCounterName());
		result.setApplication(currentDayCounter.getApplication());
		result.setDisplayed(currentDayCounter.isDisplayed());
		result.setRequestTransformPattern(currentDayCounter.getRequestTransformPattern());
		result.setMaxRequestsCount(currentDayCounter.getMaxRequestsCount());
		return result;
	}
}
