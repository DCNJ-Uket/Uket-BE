package com.uket.modules.qrcode.ticket.provider;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.uket.modules.qrcode.ticket.properties.QRCodeProperties;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class QRCodeProvider {

    private final ObjectMapper objectMapper;
    private final QRCodeProperties qrCodeProperties;

    public ByteArrayOutputStream generateQRCode(Object message) throws IOException {

        try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {

            String payload = objectMapper.writeValueAsString(message);
            BitMatrix matrix = new MultiFormatWriter().encode(payload, BarcodeFormat.QR_CODE,
                    qrCodeProperties.width(),
                    qrCodeProperties.height());

            MatrixToImageWriter.writeToStream(matrix, qrCodeProperties.type(), out);
            return out;
        } catch (WriterException exception) {
            return null;
        }
    }
}
