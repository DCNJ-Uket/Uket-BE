-- -----------------------------------------------------
-- Table `university`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS university (
                                          university_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                          name VARCHAR(255),
                                          current_event BIGINT,
                                          email_post_fix VARCHAR(255),
                                          logo_path VARCHAR(255),
                                          CONSTRAINT UK_university_name UNIQUE (name)
);

-- -----------------------------------------------------
-- Table `events`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS events (
                                      event_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                      created_at TIMESTAMP,
                                      modified_at TIMESTAMP,
                                      end_date DATE,
                                      name VARCHAR(255),
                                      start_date DATE,
                                      university_id BIGINT,
                                      location VARCHAR(255),
                                      deposit_url VARCHAR(255),
                                      CONSTRAINT FK_events_university FOREIGN KEY (university_id) REFERENCES university (university_id)
);

-- -----------------------------------------------------
-- Table `banner`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS banner (
                                      banner_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                      event_id BIGINT,
                                      path VARCHAR(255),
                                      title VARCHAR(255),
                                      CONSTRAINT FK_banner_event FOREIGN KEY (event_id) REFERENCES events (event_id)
);

-- -----------------------------------------------------
-- Table `shows`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS shows (
                                     show_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                     created_at TIMESTAMP,
                                     modified_at TIMESTAMP,
                                     end_date TIMESTAMP,
                                     location VARCHAR(255),
                                     name VARCHAR(255),
                                     start_date TIMESTAMP,
                                     ticketing_date TIMESTAMP,
                                     total_ticket_count INT,
                                     event_id BIGINT,
                                     CONSTRAINT FK_shows_event FOREIGN KEY (event_id) REFERENCES events (event_id)
);

-- -----------------------------------------------------
-- Table `reservation`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS reservation (
                                           reservation_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                           created_at TIMESTAMP,
                                           modified_at TIMESTAMP,
                                           end_time TIMESTAMP,
                                           reserved_count INT,
                                           start_time TIMESTAMP,
                                           total_count INT,
                                           type VARCHAR(20) CHECK (type IN ('TICKETING_ALL', 'TICKETING_STUDENT')),
                                           show_id BIGINT,
                                           CONSTRAINT UK_reservation_show_time UNIQUE (show_id, start_time, end_time),
                                           CONSTRAINT FK_reservation_show FOREIGN KEY (show_id) REFERENCES shows (show_id)
);

-- -----------------------------------------------------
-- Table `user_details`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS user_details (
                                            user_details_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                            depositor_name VARCHAR(255),
                                            phone_number VARCHAR(255),
                                            student_code VARCHAR(255),
                                            student_major VARCHAR(255),
                                            university_email VARCHAR(255)
);

-- -----------------------------------------------------
-- Table `users`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS users (
                                     user_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                     created_at TIMESTAMP,
                                     modified_at TIMESTAMP,
                                     email VARCHAR(255),
                                     is_registered BOOLEAN,
                                     name VARCHAR(255),
                                     phone_number VARCHAR(255),
                                     platform VARCHAR(20) CHECK (platform IN ('KAKAO', 'GOOGLE')),
                                     platform_id VARCHAR(255),
                                     role VARCHAR(20) CHECK (role IN ('ROLE_USER', 'ROLE_ADMIN')),
                                     university_id BIGINT,
                                     user_details_id BIGINT,
                                     profile_image VARCHAR(1000),
                                     CONSTRAINT FK_users_user_details FOREIGN KEY (user_details_id) REFERENCES user_details (user_details_id),
                                     CONSTRAINT FK_users_university FOREIGN KEY (university_id) REFERENCES university (university_id)
);

-- -----------------------------------------------------
-- Table `ticket`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS ticket (
                                      ticket_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                      created_at TIMESTAMP,
                                      modified_at TIMESTAMP,
                                      payment_at TIMESTAMP,
                                      status VARCHAR(20) CHECK (status IN ('BEFORE_ENTER', 'FINISH_ENTER', 'BEFORE_PAYMENT', 'RESERVATION_CANCEL', 'EXPIRED')),
                                      event_id BIGINT,
                                      reservation_id BIGINT,
                                      show_id BIGINT,
                                      user_id BIGINT,
                                      ticket_no VARCHAR(255) UNIQUE,
                                      deleted_at TIMESTAMP,
                                      enter_at TIMESTAMP,
                                      CONSTRAINT FK_ticket_user FOREIGN KEY (user_id) REFERENCES users (user_id),
                                      CONSTRAINT FK_ticket_reservation FOREIGN KEY (reservation_id) REFERENCES reservation (reservation_id),
                                      CONSTRAINT FK_ticket_show FOREIGN KEY (show_id) REFERENCES shows (show_id),
                                      CONSTRAINT FK_ticket_event FOREIGN KEY (event_id) REFERENCES events (event_id)
);
