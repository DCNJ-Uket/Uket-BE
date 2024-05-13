package com.uket.modules.qrcode.ticket.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "app.qr-code")
public record QRCodeProperties(
        int width,
        int height,
        String type

) {
}
