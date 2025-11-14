/*
 * Copyright 2008-2019 by Emeric Vernat
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

package net.bull.javamelody.internal.model;

import org.jrobin.core.RrdFileBackend;

import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.nio.channels.FileChannel;
import java.util.Timer;
import java.util.TimerTask;

/**
 * JRobin backend which is used to store RRD data to ordinary disk files
 * by using fast java.lang.foreign.* package (Foreign Function and Memory api).
 */
public class RrdFfmBackend extends RrdFileBackend {
	private static Timer fileSyncTimer;

	private Arena arena;
	private MemorySegment memorySegment;
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
	protected RrdFfmBackend(String path, boolean readOnly, int syncPeriod) throws IOException {
		super(path, readOnly);
		try {
			mapFile();
			if (!readOnly) {
				fileSyncTimer.schedule(syncTask, syncPeriod * 1000L, syncPeriod * 1000L);
			}
		} catch (final IOException ioe) {
			super.close(); // NOPMD
			throw ioe;
		} catch (final IllegalStateException e) {
			// issue #592 (IllegalStateException: Timer already cancelled)
			unmapFile();
			super.close(); // NOPMD
			throw e;
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

			arena = Arena.ofShared();
			// Arena.ofConfined() is faster, but given org.jrobin.core.RrdDbPool and in particular fileSyncTimer,
			// we can't be sure of using it in a single thread
			memorySegment = file.getChannel().map(mapMode, 0, length, arena);
		}
	}

	private void unmapFile() {
		if (arena != null) {
			arena.close();
			arena = null;
			memorySegment = null;
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
		if (memorySegment != null) {
			MemorySegment.copy(b, 0, memorySegment, ValueLayout.JAVA_BYTE, offset, b.length);
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
		if (memorySegment != null) {
			MemorySegment.copy(memorySegment, ValueLayout.JAVA_BYTE, offset, b, 0, b.length);
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
		if (memorySegment != null) {
			memorySegment.force();
		}
	}
}
