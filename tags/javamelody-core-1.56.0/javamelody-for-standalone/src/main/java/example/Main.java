package example;

import java.util.HashMap;
import java.util.Map;

import net.bull.javamelody.EmbeddedServer;
import net.bull.javamelody.Parameter;

/**
 * Example main class of a standalone app with embedded javamelody.
 */
public class Main {
	/**
	 * main.
	 * @param args String[]
	 * @throws Exception e
	 */
	public static void main(String[] args) throws Exception {
		// javamelody parameters (optional),
		final Map<Parameter, String> parameters = new HashMap<>();
		// to add basic auth:
		// parameters.put(Parameter.AUTHORIZED_USERS, "admin:pwd");

		// to change the default storage directory:
		// parameters.put(Parameter.STORAGE_DIRECTORY, "/tmp/javamelody");

		// to change the default resolution in seconds:
		// parameters.put(Parameter.RESOLUTION_SECONDS, "60");

		// to hide all statistics such as http and sql, except logs:
		// parameters.put(Parameter.DISPLAYED_COUNTERS, "log");
		// parameters.put(Parameter.NO_DATABASE, "true");

		// enable hotspots sampling with a period of 1 second:
		parameters.put(Parameter.SAMPLING_SECONDS, "1.0");

		// set the path of the reports:
		parameters.put(Parameter.MONITORING_PATH, "/");

		// start the embedded http server with javamelody
		EmbeddedServer.start(8080, parameters);
		System.out.println("");
		System.out.println("For the reports, you can browse http://localhost:8080/");
		System.out.println("");

		// compute PI just to have something to do
		System.out.print("PI = ");
		final Pi p = new Pi();
		p.calcPiDigits();
	}
}
