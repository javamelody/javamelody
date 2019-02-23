package net.bull.javamelody.internal.util;

public class MemoryUtil {

	public enum Size {
		KILO_BYTE(1024L),
		MEGA_BYTE(KILO_BYTE.byteSize * 1024L),
		GIGA_BYTE(MEGA_BYTE.byteSize * 1024L);

		private long byteSize;

		Size(long byteSize) {
			this.byteSize = byteSize;
		}

		public long getByteSize() {
			return byteSize;
		}
	}

	private MemoryUtil() {

	}

	/**
	 * Gets the used memory measured by Kilobyte, MB, or GB based in the size param.
	 * @param size
	 * @return double represent the total JVM used memory.
	 */
	public static double getUsedMemory(MemoryUtil.Size size) {
		return (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
				/ (size.byteSize * 1.0);
	}
}
