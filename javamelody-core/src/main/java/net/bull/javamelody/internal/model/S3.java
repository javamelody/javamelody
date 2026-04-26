package net.bull.javamelody.internal.model;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CompletableFuture;

import net.bull.javamelody.internal.common.LOG;
import software.amazon.awssdk.services.s3.S3AsyncClient;
import software.amazon.awssdk.services.s3.model.PutObjectResponse;

/**
 * Upload to AWS S3.
 *
 * @author Emeric Vernat
 */
final class S3 {
	private S3() {
		super();
	}

	static void upload(File file, String bucketName) throws IOException {
		// size of heap dump files may be more than 5 GB, so we need multipart support
		try (final S3AsyncClient s3AsyncClient = S3AsyncClient.builder().multipartEnabled(true).build()) {
			final CompletableFuture<PutObjectResponse> response =
				s3AsyncClient.putObject(b -> b.bucket(bucketName).key(file.getName()), file.toPath());
			response.join();
		} catch (final Exception e) {
			throw new IOException(e);
		}
		LOG.info("File " + file.getName() + " uploaded successfully to S3");
	}
}
