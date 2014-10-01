/* ============================================================
 * JRobin : Pure java implementation of RRDTool's functionality
 * ============================================================
 *
 * Project Info:  http://www.jrobin.org
 * Project Lead:  Sasa Markovic (saxon@jrobin.org);
 *
 * (C) Copyright 2003-2005, by Sasa Markovic.
 *
 * Developers:    Sasa Markovic (saxon@jrobin.org)
 *
 *
 * This library is free software; you can redistribute it and/or modify it under the terms
 * of the GNU Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this
 * library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

package net.bull.javamelody;

import java.io.IOException;

import org.jrobin.core.RrdBackend;
import org.jrobin.core.RrdFileBackendFactory;

/**
 * Factory class which creates actual {@link RrdNioBackend} objects. This is the default factory since
 * 1.4.0 version
 */
public class RrdNioBackendFactory extends RrdFileBackendFactory {
	/**
	 * factory name, "NIO-JavaMelody".
	 */
	public static final String FACTORY_NAME = "NIO-JavaMelody";

	/**
	 * Period in seconds between consecutive synchronizations when
	 * sync-mode is set to SYNC_BACKGROUND. By default in-memory cache will be
	 * transferred to the disc every 300 seconds (5 minutes). Default value can be
	 * changed via {@link #setSyncPeriod(int)} method.
	 */
	public static final int DEFAULT_SYNC_PERIOD = 300; // seconds

	private static int syncPeriod = DEFAULT_SYNC_PERIOD;

	/**
	 * Returns time between two consecutive background synchronizations. If not changed via
	 * {@link #setSyncPeriod(int)} method call, defaults to {@link #DEFAULT_SYNC_PERIOD}.
	 * See {@link #setSyncPeriod(int)} for more information.
	 *
	 * @return Time in seconds between consecutive background synchronizations.
	 */
	public static int getSyncPeriod() {
		return syncPeriod;
	}

	/**
	 * Sets time between consecutive background synchronizations.
	 *
	 * @param syncPeriod Time in seconds between consecutive background synchronizations.
	 */
	public static void setSyncPeriod(int syncPeriod) {
		RrdNioBackendFactory.syncPeriod = syncPeriod;
	}

	/**
	 * Creates RrdNioBackend object for the given file path.
	 *
	 * @param path	 File path
	 * @param readOnly True, if the file should be accessed in read/only mode.
	 *                 False otherwise.
	 * @return RrdNioBackend object which handles all I/O operations for the given file path
	 * @throws IOException Thrown in case of I/O error.
	 */
	@Override
	protected RrdBackend open(String path, boolean readOnly) throws IOException {
		return new RrdNioBackend(path, readOnly, syncPeriod);
	}

	/**
	 * Returns the name of this factory.
	 *
	 * @return Factory name (equals to string "NIO-JavaMelody")
	 */
	@Override
	public String getFactoryName() {
		return FACTORY_NAME;
	}
}
