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
package net.bull.javamelody.internal.common;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Utilitaires entrées/sorties.
 * @author Emeric Vernat
 */
public final class InputOutput {
	private InputOutput() {
		super();
	}

	public static void pump(InputStream input, OutputStream output) throws IOException {
		final byte[] bytes = new byte[4 * 1024];
		int length = input.read(bytes);
		while (length != -1) {
			output.write(bytes, 0, length);
			length = input.read(bytes);
		}
	}

	public static byte[] pumpToByteArray(InputStream input) throws IOException {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		pump(input, out);
		return out.toByteArray();
	}

	public static String pumpToString(InputStream input, Charset charset) throws IOException {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		pump(input, out);
		return out.toString(charset.name());
	}

	public static void pumpToFile(InputStream input, File file) throws IOException {
		final OutputStream output = new FileOutputStream(file);
		try {
			pump(input, output);
		} finally {
			output.close();
		}
	}

	public static void pumpFromFile(File file, OutputStream output) throws IOException {
		final FileInputStream in = new FileInputStream(file);
		try {
			pump(in, output);
		} finally {
			in.close();
		}
	}

	public static void zipFile(File source, File target) throws IOException {
		final FileOutputStream fos = new FileOutputStream(target);
		final ZipOutputStream zos = new ZipOutputStream(fos);
		try {
			final ZipEntry ze = new ZipEntry(source.getName());
			zos.putNextEntry(ze);
			pumpFromFile(source, zos);
			zos.closeEntry();
		} finally {
			zos.close();
		}
	}

	public static boolean deleteFile(File file) {
		return file.delete();
	}

	public static void copyFile(File source, File target) throws IOException {
		final FileInputStream in = new FileInputStream(source);
		final FileOutputStream out = new FileOutputStream(target);
		try {
			pump(in, out);
		} finally {
			out.close();
			in.close();
		}
	}
}
