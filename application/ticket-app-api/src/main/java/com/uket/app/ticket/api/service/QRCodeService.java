package com.uket.app.ticket.api.service;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import com.uket.modules.jwt.util.JwtTicketUtil;
import com.uket.modules.qrcode.ticket.provider.QRCodeProvider;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class QRCodeService {

    private final QRCodeProvider qrCodeProvider;
    private final JwtTicketUtil jwtTicketUtil;

    public byte[] generateTicketQRCode(Long ticketId) {

        String ticketToken = jwtTicketUtil.createTicketToken(ticketId);

        try (ByteArrayOutputStream out = qrCodeProvider.generateQRCodeByString(ticketToken)) {
            validateQRCode(out);
            return out.toByteArray();
        } catch (IOException e) {
            throw new BaseException(ErrorCode.IO_ERROR);
        }
    }

    private void validateQRCode(ByteArrayOutputStream out) {
        if (out == null) {
            throw new BaseException(ErrorCode.FAIL_TO_GENERATE_QRCODE);
        }
    }
}
