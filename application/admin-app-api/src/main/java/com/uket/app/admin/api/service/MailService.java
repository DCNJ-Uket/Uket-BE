package com.uket.app.admin.api.service;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class MailService {

    private final JavaMailSender javaMailSender;

    public void sendEmail(String to, String subject, String text) {
        SimpleMailMessage emailForm = createEmailForm(to, subject, text);
        try {
            javaMailSender.send(emailForm);
        } catch (RuntimeException e) {
            throw new BaseException(ErrorCode.UNABLE_TO_SEND_EMAIL);
        }
    }

    public void sendEmailWithHtmlContent(String to, String subject, String htmlContent)
        throws MessagingException {
        MimeMessage htmlEmailForm = createHtmlEmailForm(to,subject,htmlContent);
        try {
            javaMailSender.send(htmlEmailForm);
        } catch (Exception e) {
            throw new BaseException(ErrorCode.UNABLE_TO_SEND_EMAIL);
        }
    }

    private SimpleMailMessage createEmailForm(String to, String subject, String text) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setTo(to);
        message.setSubject(subject);
        message.setText(text);

        return message;
    }

    private MimeMessage createHtmlEmailForm(String to, String subject, String htmlContent)
        throws MessagingException {
        MimeMessage message = javaMailSender.createMimeMessage();

        MimeMessageHelper helper = new MimeMessageHelper(message, false, "UTF-8");
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(htmlContent, true);

        return message;
    }
}
