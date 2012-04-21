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

import java.awt.Color;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Paint;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Calendar;
import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;

import org.jrobin.core.RrdDb;
import org.jrobin.core.RrdDbPool;
import org.jrobin.core.RrdDef;
import org.jrobin.core.RrdException;
import org.jrobin.core.RrdNioBackend;
import org.jrobin.core.Sample;
import org.jrobin.core.Util;
import org.jrobin.graph.RrdGraph;
import org.jrobin.graph.RrdGraphDef;

/**
 * Stockage RRD et graphiques statistiques.
 * Cette classe utilise JRobin (http://www.jrobin.org/index.php/Main_Page)
 * qui est une librairie Java opensource (LGPL) similaire à RRDTool (http://oss.oetiker.ch/rrdtool/).
 * L'API et le tutorial JRobin sont à http://oldwww.jrobin.org/api/index.html
 * @author Emeric Vernat
 */
final class JRobin {
	static final int SMALL_HEIGHT = 50;
	private static final Color LIGHT_RED = Color.RED.brighter().brighter();
	private static final Paint SMALL_GRADIENT = new GradientPaint(0, 0, LIGHT_RED, 0, SMALL_HEIGHT,
			Color.GREEN, false);
	private static final int HOUR = 60 * 60;
	private static final int DAY = 24 * HOUR;

	// pool of open RRD files
	private final RrdDbPool rrdPool = RrdDbPool.getInstance();
	private final String application;
	private final String name;
	private final String rrdFileName;
	private final int step;
	private final String requestName;

	private JRobin(String application, String name, File rrdFile, int step, String requestName)
			throws RrdException, IOException {
		super();
		assert application != null;
		assert name != null;
		assert rrdFile != null;
		assert step > 0;
		// requestName est null pour un compteur

		this.application = application;
		this.name = name;
		this.rrdFileName = rrdFile.getPath();
		this.step = step;
		this.requestName = requestName;

		init();
	}

	static void stop() throws IOException {
		getJRobinFileSyncTimer().cancel();
	}

	static void setJRobinThreadName(final String jrobinThreadName) throws IOException {
		final TimerTask threadNameTask = new TimerTask() {
			/** {@inheritDoc} */
			@Override
			public void run() {
				Thread.currentThread().setName(jrobinThreadName);
			}
		};
		getJRobinFileSyncTimer().schedule(threadNameTask, 0);
	}

	static Timer getJRobinFileSyncTimer() throws IOException {
		try {
			// on accède à ce timer par réflexion pour l'arrêter faute d'autre moyen
			final Field field = RrdNioBackend.class.getDeclaredField("fileSyncTimer");
			setFieldAccessible(field);
			return (Timer) field.get(null);
		} catch (final NoSuchFieldException e) {
			throw createIOException(e);
		} catch (final IllegalAccessException e) {
			throw createIOException(e);
		}
	}

	private static void setFieldAccessible(final Field field) {
		AccessController.doPrivileged(new PrivilegedAction<Object>() { // pour findbugs
					/** {@inheritDoc} */
					public Object run() {
						field.setAccessible(true);
						return null;
					}
				});
	}

	static JRobin createInstance(String application, String name, String requestName)
			throws IOException {
		final File dir = Parameters.getStorageDirectory(application);
		final File rrdFile = new File(dir, name + ".rrd");
		final int step = Parameters.getResolutionSeconds();
		try {
			return new JRobin(application, name, rrdFile, step, requestName);
		} catch (final RrdException e) {
			throw createIOException(e);
		}
	}

	private void init() throws IOException, RrdException {
		final File rrdFile = new File(rrdFileName);
		final File rrdDirectory = rrdFile.getParentFile();
		if (!rrdDirectory.mkdirs() && !rrdDirectory.exists()) {
			throw new IOException("JavaMelody directory can't be created: "
					+ rrdDirectory.getPath());
		}
		// cf issue 41: rrdFile could have been created with length 0 if out of disk space
		// (fix IOException: Read failed, file xxx.rrd not mapped for I/O)
		if (!rrdFile.exists() || rrdFile.length() == 0) {
			// create RRD file since it does not exist (or is empty)
			final RrdDef rrdDef = new RrdDef(rrdFileName, step);
			// "startTime" décalé de "step" pour éviter que addValue appelée juste
			// après ne lance l'exception suivante la première fois
			// "Bad sample timestamp x. Last update time was x, at least one second step is required"
			rrdDef.setStartTime(Util.getTime() - step);
			// single gauge datasource
			final String dsType = "GAUGE";
			// max time before "unknown value"
			final int heartbeat = step * 2;
			rrdDef.addDatasource(getDataSourceName(), dsType, heartbeat, 0, Double.NaN);
			// several archives
			final String average = "AVERAGE";
			final String max = "MAX";
			// 1 jour
			rrdDef.addArchive(average, 0.25, 1, DAY / step);
			rrdDef.addArchive(max, 0.25, 1, DAY / step);
			// 1 semaine
			rrdDef.addArchive(average, 0.25, HOUR / step, 7 * 24);
			rrdDef.addArchive(max, 0.25, HOUR / step, 7 * 24);
			// 1 mois
			rrdDef.addArchive(average, 0.25, 6 * HOUR / step, 31 * 4);
			rrdDef.addArchive(max, 0.25, 6 * HOUR / step, 31 * 4);
			// 2 ans (1 an pour période "1 an" et 2 ans pour période "tout")
			rrdDef.addArchive(average, 0.25, 8 * 6 * HOUR / step, 2 * 12 * 15);
			rrdDef.addArchive(max, 0.25, 8 * 6 * HOUR / step, 2 * 12 * 15);
			// create RRD file in the pool
			final RrdDb rrdDb = rrdPool.requestRrdDb(rrdDef);
			rrdPool.release(rrdDb);
		}
	}

	private void resetFile() throws IOException {
		deleteFile();
		try {
			init();
		} catch (final RrdException e) {
			throw createIOException(e);
		}
	}

	byte[] graph(Range range, int width, int height) throws IOException {
		try {
			// Rq : il pourrait être envisagé de récupérer les données dans les fichiers rrd ou autre stockage
			// puis de faire des courbes en sparklines html (sauvegardées dans la page html)
			// ou avec http://code.google.com/apis/chart/types.html#sparkline ou jfreechart

			// create common part of graph definition
			final RrdGraphDef graphDef = new RrdGraphDef();
			if (Locale.CHINESE.getLanguage().equals(
					I18N.getResourceBundle().getLocale().getLanguage())) {
				graphDef.setSmallFont(new Font(Font.MONOSPACED, Font.PLAIN, 10));
				graphDef.setLargeFont(new Font(Font.MONOSPACED, Font.BOLD, 12));
			}

			initGraphSource(graphDef, height);

			initGraphPeriodAndSize(range, width, height, graphDef);

			graphDef.setImageFormat("png");
			graphDef.setFilename("-");
			// il faut utiliser le pool pour les performances
			// et pour éviter des erreurs d'accès concurrents sur les fichiers
			// entre différentes générations de graphs et aussi avec l'écriture des données
			graphDef.setPoolUsed(true);
			return new RrdGraph(graphDef).getRrdGraphInfo().getBytes();
		} catch (final RrdException e) {
			throw createIOException(e);
		}
	}

	private void initGraphPeriodAndSize(Range range, int width, int height, RrdGraphDef graphDef) {
		// ending timestamp is the (current) timestamp in seconds
		// starting timestamp will be adjusted for each graph
		final long endTime;
		final long startTime;
		if (range.getPeriod() == null) {
			// si endDate à la date du jour, alors on ne dépasse pas l'heure courante
			endTime = Math.min(range.getEndDate().getTime() / 1000, Util.getTime());
			startTime = range.getStartDate().getTime() / 1000;
		} else {
			endTime = Util.getTime();
			startTime = endTime - range.getPeriod().getDurationSeconds();
		}
		final String label = getLabel();
		final String titleStart;
		if (label.length() > 31 && width <= 200) {
			// si le label est trop long, on raccourci le titre sinon il ne rentre pas
			titleStart = label;
		} else {
			titleStart = label + " - " + range.getLabel();
		}
		final String titleEnd;
		if (width > 400) {
			if (range.getPeriod() == null) {
				titleEnd = " - " + I18N.getFormattedString("sur", getApplication());
			} else {
				titleEnd = " - " + I18N.getCurrentDate() + ' '
						+ I18N.getFormattedString("sur", getApplication());
			}
		} else {
			titleEnd = "";
			if (range.getPeriod() == null) {
				// si période entre 2 dates et si pas de zoom,
				// alors on réduit de 2 point la fonte du titre pour qu'il rentre dans le cadre
				graphDef.setLargeFont(graphDef.getLargeFont().deriveFont(
						graphDef.getLargeFont().getSize2D() - 2f));
			}
		}
		graphDef.setStartTime(startTime);
		graphDef.setEndTime(endTime);
		graphDef.setTitle(titleStart + titleEnd);
		graphDef.setFirstDayOfWeek(Calendar.getInstance(I18N.getCurrentLocale())
				.getFirstDayOfWeek());
		// or if the user locale patch is merged we should do:
		// (https://sourceforge.net/tracker/?func=detail&aid=3403733&group_id=82668&atid=566807)
		//graphDef.setLocale(I18N.getCurrentLocale());

		// rq : la largeur et la hauteur de l'image sont plus grandes que celles fournies
		// car jrobin ajoute la largeur et la hauteur des textes et autres
		graphDef.setWidth(width);
		graphDef.setHeight(height);
		if (width <= 100) {
			graphDef.setNoLegend(true);
			graphDef.setUnitsLength(0);
			graphDef.setShowSignature(false);
			graphDef.setTitle(null);
		}
		//		graphDef.setColor(RrdGraphConstants.COLOR_BACK, new GradientPaint(0, 0,
		//				RrdGraphConstants.DEFAULT_BACK_COLOR.brighter(), 0, height,
		//				RrdGraphConstants.DEFAULT_BACK_COLOR));
	}

	private void initGraphSource(RrdGraphDef graphDef, int height) {
		final String average = "average";
		final String max = "max";
		final String dataSourceName = getDataSourceName();
		graphDef.datasource(average, rrdFileName, dataSourceName, "AVERAGE");
		graphDef.datasource(max, rrdFileName, dataSourceName, "MAX");
		graphDef.setMinValue(0);
		final String moyenneLabel = I18N.getString("Moyenne");
		final String maximumLabel = I18N.getString("Maximum");
		graphDef.area(average, getPaint(height), moyenneLabel);
		graphDef.line(max, Color.BLUE, maximumLabel);
		graphDef.gprint(average, "AVERAGE", moyenneLabel + ": %9.0f %S\\r");
		//graphDef.gprint(average, "MIN", "Minimum: %9.0f %S\\r");
		graphDef.gprint(max, "MAX", maximumLabel + ": %9.0f %S\\r");
		// graphDef.comment("JRobin :: RRDTool Choice for the Java World");
	}

	private static Paint getPaint(int height) {
		// si on avait la moyenne globale/glissante des valeurs et l'écart type
		// on pourrait mettre vert si < moyenne + 1 écart type puis orange puis rouge si > moyenne + 2 écarts types,
		// en utilisant LinearGradientPaint par exemple, ou bien selon paramètres de plages de couleurs par graphe
		if (height == SMALL_HEIGHT) {
			// design pattern fly-weight (poids-mouche) pour le cas des 9 ou 12 graphs
			// dans la page html de départ
			return SMALL_GRADIENT;
		}
		return new GradientPaint(0, 0, LIGHT_RED, 0, height, Color.GREEN, false);
	}

	void addValue(double value) throws IOException {
		try {
			// request RRD database reference from the pool
			final RrdDb rrdDb = rrdPool.requestRrdDb(rrdFileName);
			synchronized (rrdDb) {
				try {
					// create sample with the current timestamp
					final Sample sample = rrdDb.createSample();
					// test pour éviter l'erreur suivante au redéploiement par exemple:
					// org.jrobin.core.RrdException:
					// Bad sample timestamp x. Last update time was x, at least one second step is required
					if (sample.getTime() > rrdDb.getLastUpdateTime()) {
						// set value for load datasource
						sample.setValue(getDataSourceName(), value);
						// update database
						sample.update();
					}
				} finally {
					// release RRD database reference
					rrdPool.release(rrdDb);
				}
			}
		} catch (final RrdException e) {
			if (e.getMessage() != null && e.getMessage().startsWith("Invalid file header")) {
				// le fichier RRD a été corrompu, par exemple en tuant le process java au milieu
				// d'un write, donc on efface le fichier corrompu et on le recrée pour corriger
				// le problème
				resetFile();
				addValue(value);
			}
			throw createIOException(e);
		}
	}

	double getLastValue() throws IOException {
		try {
			// request RRD database reference from the pool
			final RrdDb rrdDb = rrdPool.requestRrdDb(rrdFileName);
			try {
				return rrdDb.getLastDatasourceValue(getDataSourceName());
			} finally {
				// release RRD database reference
				rrdPool.release(rrdDb);
			}
		} catch (final RrdException e) {
			throw createIOException(e);
		}
	}

	boolean deleteFile() {
		return new File(rrdFileName).delete();
	}

	private String getApplication() {
		return application;
	}

	String getName() {
		return name;
	}

	private String getDataSourceName() {
		// RrdDef.addDatasource n'accepte pas un nom de datasource supérieur à 20 caractères
		return name.substring(0, Math.min(20, name.length()));
	}

	String getLabel() {
		if (requestName == null) {
			// c'est un jrobin global issu soit de JavaInformations soit d'un Counter dans le Collector
			return I18N.getString(getName());
		}
		// c'est un jrobin issu d'un CounterRequest dans le Collector
		final String shortRequestName = requestName
				.substring(0, Math.min(30, requestName.length()));
		// plus nécessaire:  if (getName().startsWith("error")) {
		// c'est un jrobin issu d'un CounterRequest du Counter "error"
		// return I18N.getString("Erreurs_par_minute_pour") + ' ' + shortRequestName; }
		return I18N.getFormattedString("Temps_moyens_de", shortRequestName);
	}

	private static IOException createIOException(Exception e) {
		// Rq: le constructeur de IOException avec message et cause n'existe qu'en jdk 1.6
		final IOException ex = new IOException(e.getMessage());
		ex.initCause(e);
		return ex;
	}

	static boolean deleteObsoleteJRobinFiles(String application) throws IOException {
		final File storageDir = Parameters.getStorageDirectory(application);
		final Calendar nowMinusThreeMonthsAndADay = Calendar.getInstance();
		nowMinusThreeMonthsAndADay.add(Calendar.MONTH, -3);
		nowMinusThreeMonthsAndADay.add(Calendar.DAY_OF_YEAR, -1);
		final long timestamp = Util.getTimestamp(nowMinusThreeMonthsAndADay);
		boolean result = true;
		// filtre pour ne garder que les fichiers d'extension .rrd et pour éviter d'instancier des File inutiles
		final FilenameFilter filenameFilter = new FilenameFilter() {
			/** {@inheritDoc} */
			public boolean accept(File dir, String fileName) {
				return fileName.endsWith(".rrd");
			}
		};
		final RrdDbPool rrdPool;
		try {
			rrdPool = RrdDbPool.getInstance();
		} catch (final RrdException e) {
			throw createIOException(e);
		}
		final int counterRequestIdLength = new CounterRequest("", "").getId().length();
		for (final File file : storageDir.listFiles(filenameFilter)) {
			// on ne supprime que les fichiers rrd de requêtes (les autres sont peu nombreux)
			if (file.getName().length() > counterRequestIdLength
					&& file.lastModified() < nowMinusThreeMonthsAndADay.getTimeInMillis()) {
				try {
					final RrdDb rrdDb = rrdPool.requestRrdDb(file.getPath());
					final boolean obsolete = rrdDb.getLastUpdateTime() < timestamp;
					rrdPool.release(rrdDb);
					if (obsolete) {
						result = result && file.delete();
					}
				} catch (final IOException e) {
					continue;
				} catch (final RrdException e) {
					continue;
				}
			}
		}

		// on retourne true si tous les fichiers .rrd obsolètes ont été supprimés, false sinon
		return result;
	}

	//  public void test() throws RrdException, IOException {
	//    for (int i = 1000; i > 0; i--) {
	//      // request RRD database reference from the pool
	//      RrdDb rrdDb = rrdPool.requestRrdDb(rrdFileName);
	//      // create sample with the current timestamp
	//      Sample sample = rrdDb.createSample(Util.getTime() - 120 * i);
	//      // set value for load datasource
	//      // println(i + " " + new byte[5000]);
	//      sample.setValue(name, Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	//      // update database
	//      sample.update();
	//      // release RRD database reference
	//      rrdPool.release(rrdDb);
	//    }
	//
	//    graph(Period.JOUR);
	//    graph(Period.SEMAINE);
	//    graph(Period.MOIS);
	//    graph(Period.ANNEE);
	//  }
	//
	//  public static void main(String[] args) throws IOException, RrdException {
	//    new JRobin("Mémoire", "jrobin", 120).test();
	//  }
}
