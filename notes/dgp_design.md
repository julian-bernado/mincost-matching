# Sam & Yizhou

---

## Design DGP

### Functional form

- **Linear functional form?**
    - *Just noise*  
        - $ y_i = \epsilon_i $, $ \epsilon_i \sim \text{wn}(\sigma^2) $
    - *Linear model, no effect*  
        - $ y_i = \beta^T x_i + \epsilon_i $, $ x_i \sim D $  
    - *Linear model, constant effect*  
        - $ y_i = \beta^T x_i + \tau \cdot 1(z_i = 1) + \epsilon_i $  
    - *Linear model, heterogeneous effects*  
        - $ y_i = \beta^T x_i + \tau \cdot 1(z_i = 1) + \beta_{\tau}^T x_i \cdot 1(z_i = 1) + \epsilon_i $  

### Covariates

- **Types & distributions:**
    - Real-valued (normal, Cauchy)
    - Categorical/multinomial
    - Bounded (uniform)
    - Positive (exponential)
    - $ \mathbb{Z}^+ $ (Poisson)

- **Encode distance between covariates for treatment and control**  
    - Conditional on treatment assignment, generate covariates (maybe shift means and variances, change distributions?)  

- **How many covariates?**  
    - Vary across DGPs, but fixed within each one  
