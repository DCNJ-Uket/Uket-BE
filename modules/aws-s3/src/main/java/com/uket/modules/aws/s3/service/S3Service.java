package com.uket.modules.aws.s3.service;

import com.uket.modules.aws.s3.properties.S3Properties;
import java.time.Duration;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;
import software.amazon.awssdk.services.s3.presigner.model.PresignedGetObjectRequest;

@Service
@RequiredArgsConstructor
public class S3Service {

    private static final String UNIVERSITY_LOGO_FOLDER = "university-logo";

    private final S3Presigner preSigner;
    private final S3Properties s3Properties;

    public String getUniversityLogo(String filename) {
        return getPreSignedUrl(UNIVERSITY_LOGO_FOLDER, filename);
    }

    private String getPreSignedUrl(String folder, String filename) {
        if (filename == null || filename.isEmpty()) {
            return null;
        }

        GetObjectPresignRequest getObjectPresignRequest = GetObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(1))
                .getObjectRequest(objectRequest ->
                        objectRequest
                                .bucket(s3Properties.bucket())
                                .key(String.join("/", folder, filename)))
                .build();

        PresignedGetObjectRequest presignedGetObjectRequest = preSigner
                .presignGetObject(getObjectPresignRequest);

        String url = presignedGetObjectRequest.url().toString();

        preSigner.close();
        return url;
    }
}
