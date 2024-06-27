package com.uket.app.ticket.api.service;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
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

    private SimpleMailMessage createEmailForm(String to, String subject, String text) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setTo(to);
        message.setSubject(subject);
        message.setText(text);

        return message;
    }
}
