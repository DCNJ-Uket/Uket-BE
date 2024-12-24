-- Remove depositUrl column from Events table
ALTER TABLE events DROP COLUMN deposit_url;

-- Create Account table
CREATE TABLE account (
                         account_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                         deposit_url VARCHAR(255) NOT NULL,
                         account_number VARCHAR(50),
                         account_owner VARCHAR(100),
                         ticket_price INT,
                         created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
                         modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- Add account_id column to Events table
ALTER TABLE events ADD COLUMN account_id BIGINT;

-- Add foreign key constraint between Events and Account
ALTER TABLE events
    ADD CONSTRAINT fk_events_account
        FOREIGN KEY (account_id) REFERENCES account(account_id);
