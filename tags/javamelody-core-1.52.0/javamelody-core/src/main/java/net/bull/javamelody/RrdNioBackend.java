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
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Timer;
import java.util.TimerTask;

import org.jrobin.core.RrdFileBackend;

import sun.nio.ch.DirectBuffer;

/**
 * JRobin backend which is used to store RRD data to ordinary disk files
 * by using fast java.nio.* package. This is the default backend engine since JRobin 1.4.0.
 */
public class RrdNioBackend extends RrdFileBackend {
	private static Timer fileSyncTimer;

	private MappedByteBuffer byteBuffer;
	private final TimerTask syncTask = new TimerTask() {
		@Override
		public void run() {
			sync();
		}
	};

	/**
	 * Creates RrdFileBackend object for the given file path, backed by java.nio.* classes.
	 *
	 * @param path	   Path to a file
	 * @param readOnly   True, if file should be open in a read-only mode. False otherwise
	 * @param syncPeriod See {@link RrdNioBackendFactory#setSyncPeriod(int)} for explanation
	 * @throws IOException Thrown in case of I/O error
	 */
	protected RrdNioBackend(String path, boolean readOnly, int syncPeriod) throws IOException {
		super(path, readOnly);
		try {
			mapFile();
			if (!readOnly) {
				fileSyncTimer.schedule(syncTask, syncPeriod * 1000L, syncPeriod * 1000L);
			}
		} catch (final IOException ioe) {
			super.close();
			throw ioe;
		}
	}

	/**
	 * @return The timer to synchronize files.
	 */
	public static Timer getFileSyncTimer() {
		return fileSyncTimer;
	}

	/**
	 * Sets the timer.
	 * @param timer timer to synchronize files.
	 */
	public static void setFileSyncTimer(Timer timer) {
		fileSyncTimer = timer;
	}

	private void mapFile() throws IOException {
		final long length = getLength();
		if (length > 0) {
			final FileChannel.MapMode mapMode =
			// (issue 328) readOnly ? FileChannel.MapMode.READ_ONLY :
			FileChannel.MapMode.READ_WRITE;
			byteBuffer = file.getChannel().map(mapMode, 0, length);
		}
	}

	private void unmapFile() {
		if (byteBuffer != null) {
			if (byteBuffer instanceof DirectBuffer) {
				((DirectBuffer) byteBuffer).cleaner().clean();
			}
			byteBuffer = null;
		}
	}

	/**
	 * Sets length of the underlying RRD file. This method is called only once, immediately
	 * after a new RRD file gets created.
	 *
	 * @param newLength Length of the RRD file
	 * @throws IOException Thrown in case of I/O error.
	 */
	@Override
	protected synchronized void setLength(long newLength) throws IOException {
		unmapFile();
		super.setLength(newLength);
		mapFile();
	}

	/**
	 * Writes bytes to the underlying RRD file on the disk
	 *
	 * @param offset Starting file offset
	 * @param b	  Bytes to be written.
	 */
	@Override
	protected synchronized void write(long offset, byte[] b) throws IOException {
		if (byteBuffer != null) {
			byteBuffer.position((int) offset);
			byteBuffer.put(b);
		} else {
			throw new IOException("Write failed, file " + getPath() + " not mapped for I/O");
		}
	}

	/**
	 * Reads a number of bytes from the RRD file on the disk
	 *
	 * @param offset Starting file offset
	 * @param b	  Buffer which receives bytes read from the file.
	 */
	@Override
	protected synchronized void read(long offset, byte[] b) throws IOException {
		if (byteBuffer != null) {
			byteBuffer.position((int) offset);
			byteBuffer.get(b);
		} else {
			throw new IOException("Read failed, file " + getPath() + " not mapped for I/O");
		}
	}

	/**
	 * Closes the underlying RRD file.
	 *
	 * @throws IOException Thrown in case of I/O error
	 */
	@Override
	public synchronized void close() throws IOException {
		// cancel synchronization
		try {
			if (syncTask != null) {
				syncTask.cancel();
			}
			sync();
			unmapFile();
		} finally {
			super.close();
		}
	}

	/**
	 * This method forces all data cached in memory but not yet stored in the file,
	 * to be stored in it.
	 */
	protected synchronized void sync() {
		if (byteBuffer != null) {
			byteBuffer.force();
		}
	}
}
