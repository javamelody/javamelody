package net.bull.javamelody;

import java.awt.Desktop;
import java.io.File;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.*;

import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.DataMerge;

/**
 * Main class of a standalone app with embedded javamelody to view local data.
 * For example, without access to the online reports, a snapshot copy of data files is made then reports are viewed later and offline with this tool.
 */
public class Viewer {
	/**
	 * main.
	 * @param args String[]
	 * @throws Exception e
	 */
	public static void main(String[] args) throws Exception {
		final String storageDirectory = Parameter.STORAGE_DIRECTORY.getValue();
		if (storageDirectory == null) {
			throw new IllegalArgumentException(
					"Please give the javamelody storage directory with -Djavamelody.storage-directory=... containing directories with the data of one or more instances of an application");
		}
		// merge and copy the data of one or more instances into a temporary directory
		final String tmpApplication = "tmpjavamelody" + new Random().nextInt();
		final String mergedDirectory = System.getProperty("java.io.tmpdir"); //Parameters.getStorageDirectory(tmpApplication).getPath();

 		final String mergedApplicationDirectory = mergedDirectory + '/' + tmpApplication;
		DataMerge.main(new String[] { storageDirectory, mergedApplicationDirectory});
		addShutdownHook(new File(mergedApplicationDirectory));

		final Map<Parameter, String> parameters = new HashMap<>();
		// set the path of the reports:
		parameters.put(Parameter.MONITORING_PATH, "/");

		// set the storage directory and temp application name:
		Parameter.STORAGE_DIRECTORY.setValue(mergedDirectory);
		parameters.put(Parameter.APPLICATION_NAME, tmpApplication);

		// start the embedded http server with javamelody
		final String port = System.getProperty("javamelody.viewer.port", "8080");
		String url = "http://localhost:" + port + '/';
		System.out.println("Starting on " + url);
		final MonitoringFilter monitoringFilter = EmbeddedServer.start(Integer.parseInt(port), parameters);

		// display stats like spring and services when files are present
		updateDisplayedCounters(
				monitoringFilter.getFilterContext().getCollector().getCounters(), new File(mergedApplicationDirectory));

		// open the reports in a browser
		final String lastDay = new SimpleDateFormat("yyyy-MM-dd")
				.format(new Date(getLatest(new File(storageDirectory))));
		url += "?period=" + lastDay + "%7C" + lastDay + "&pattern=yyyy-MM-dd";
		System.out.println("Opening the reports in a browser on " + url);
		Desktop.getDesktop().browse(URI.create(url));
		System.out.println("Done");
	}

	private static void updateDisplayedCounters(List<Counter> counters, File mergedApplicationDirectory) {
		final List<String> displayedCounters = new ArrayList<>();
		for (final Counter counter : counters) {
			if (counter.isDisplayed()
					|| new File(mergedApplicationDirectory, counter.getName() + ".ser.gz").exists()) {
				counter.setDisplayed(true);
				displayedCounters.add(counter.getName());
			}
		}
		System.out.println("Displayed counters: " + displayedCounters);
	}

	private static long getLatest(File directory) {
		long latest = 0;
		for (final File file : directory.listFiles()) {
			if (file.isDirectory()) {
				latest = Math.max(latest, getLatest(file));
			} else {
				latest = Math.max(latest, file.lastModified());
			}
		}
		return latest;
	}

	private static void addShutdownHook(final File directoryToCleanup) {
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			System.out.println("Cleaning up...");
			try {
				// stop is needed to remove locks on files such as the javamelody.lock file
				EmbeddedServer.stop();
			} catch (final Exception e) {
				System.out.println(e);
			}
			if (directoryToCleanup.exists()) {
				for (final File file : directoryToCleanup.listFiles()) {
					file.delete();
				}
				directoryToCleanup.delete();
			}
			System.out.println("Good bye");
		}));
	}
}
