# Blood Donation Token Incentive System

A blockchain-based reward system that incentivizes blood donation through token rewards.

## 🎯 Features

- ✨ Register as a blood donor
- 💉 Record blood donations
- 🎁 Earn tokens for each donation
- 💸 Transfer tokens between users
- ⏰ Enforced cooling period between donations
- 👥 Donor eligibility tracking

## 🚀 Usage

### For Donors

1. Register as a donor:
```clarity
(contract-call? .blood-donation-token-incentive-system register-donor)
```

2. Record a donation:
```clarity
(contract-call? .blood-donation-token-incentive-system record-donation)
```

3. Check your token balance:
```clarity
(contract-call? .blood-donation-token-incentive-system get-balance tx-sender)
```

4. Transfer tokens:
```clarity
(contract-call? .blood-donation-token-incentive-system transfer amount sender recipient)
```

### For Administrators

- Update tokens per donation:
```clarity
(contract-call? .blood-donation-token-incentive-system update-tokens-per-donation new-amount)
```

## 🔒 Security Features

- Cooling period between donations
- Owner-only administrative functions
- Validation checks for all operations

## 🤝 Contributing

Feel free to submit issues and enhancement requests!
```

