Permuted Block Randomisation
================
Alessia Tosi
15/01/2018

### Aims

In this script,

1.  I perform a stratified randomisation, with Month of first selection (Jan-April) and business size (0-4, 5-9, and 10-19) as prognostic (i.e., stratifying) variables. This produces 12 strata which will then be taken into account in the analysis model.

2.  I then use a permuted block randomisation with varying block sizes process to randomly assign businesses to condition within each strata. That means:

    -   Businesses are randomly assigned with a 1:1 ratio to Control vs. Intervention.
    -   I perform the assignment according to a computer-generated random sequence in permuted block of (2,4,6,8) within each strata.

    An example of one possible of such sequences (where B/C stands for BI intervention/Control, one letter = one business = one assignment): \[C B\] \[B C\] \[C B B C B C\] \[C B\] \[B B C B B C C C\] ..

### Why are these two steps important?

1.  Stratified randomisation accounts for possible baseline imbalance of potentially confounding variables, and

2.  Permuted block randomisation ensures that we get an equal number of units (i.e., businesses) in each condition (Control vs. Intervention).

### Data

Data has been previously-preprocessed and fully anonymised.

### 1. Stratification

Let us reshuffle the rows in the dataset.

``` r
set.seed(42)
bsn_df <- bsn_df[sample(nrow(bsn_df)),]
```

Split data into individual strata datasets (one for each strata)

``` r
list_of_strata_df <- split(bsn_df, list(factor(bsn_df$newly_selected_month), factor(bsn_df$size)))
```

Let's take a look at one:

Create lookup named vector with the names of the strata as names, and the number of cases in each strata as elements. This vector will be the main input to the `?blockrand::blockrand` function (as `n`, the "number of subjects to randomise") that we will use for our randomisation.

``` r
strata_table <- bsn_df %>% 
      group_by(newly_selected_month, size) %>% 
      count()

strata_table$stratum <- with(strata_table, interaction(newly_selected_month, size))

# create a lookup named vector with stratum as name, and size of stratum n as elements
strata_vec <- setNames(strata_table[['n']], as.character(strata_table[['stratum']]))
```

    ##    Apr.0-4 Apr.05-Sep Apr.Oct-19    Feb.0-4 Feb.05-Sep Feb.Oct-19 
    ##        314        110         33        231         80         20 
    ##    Jan.0-4 Jan.05-Sep Jan.Oct-19    Mar.0-4 Mar.05-Sep Mar.Oct-19 
    ##        338        272         75        271        112         34

### 2. Randomisation

Stratified randomisation with block random assignment to condition within each strata (with varying-size blocks)

We use the `blockrand::blockrand` function which uses the `sample` function internally to generate the blocks. We use `set.seed` to define the RNG seed to enable us to reproduce the block sequencing in the future.

``` r
blockrand_by_strata_fun <- function(strata_container, strata_name = '', conditions_n=2, conditions_names = c('Control', 'BI')){
      
      # create random sequence
      random_seq <- blockrand::blockrand(
            
            n = strata_container[[strata_name]], 
            num.levels = conditions_n, 
            levels = conditions_names,
            id.prefix = strata_name, 
            block.prefix = strata_name, 
            stratum = strata_name
            
            )
      
      # number of randomizations may end up being more than n (see ?blockrand) so this is to make sure it's of the right length:
      random_seq[1:strata_container[[strata_name]],]
      
}
```

``` r
## JANUARY
blockrand_by_strata_fun(strata_container=strata_vec, strata_name = 'Jan.0-4', conditions_n=2, conditions_names = c('Control', 'BI'))
```

    ##             id stratum  block.id block.size treatment
    ## 1   Jan.0-4001 Jan.0-4 Jan.0-401          6   Control
    ## 2   Jan.0-4002 Jan.0-4 Jan.0-401          6   Control
    ## 3   Jan.0-4003 Jan.0-4 Jan.0-401          6        BI
    ## 4   Jan.0-4004 Jan.0-4 Jan.0-401          6        BI
    ## 5   Jan.0-4005 Jan.0-4 Jan.0-401          6        BI
    ## 6   Jan.0-4006 Jan.0-4 Jan.0-401          6   Control
    ## 7   Jan.0-4007 Jan.0-4 Jan.0-402          2   Control
    ## 8   Jan.0-4008 Jan.0-4 Jan.0-402          2        BI
    ## 9   Jan.0-4009 Jan.0-4 Jan.0-403          8   Control
    ## 10  Jan.0-4010 Jan.0-4 Jan.0-403          8        BI
    ## 11  Jan.0-4011 Jan.0-4 Jan.0-403          8        BI
    ## 12  Jan.0-4012 Jan.0-4 Jan.0-403          8   Control
    ## 13  Jan.0-4013 Jan.0-4 Jan.0-403          8        BI
    ## 14  Jan.0-4014 Jan.0-4 Jan.0-403          8   Control
    ## 15  Jan.0-4015 Jan.0-4 Jan.0-403          8        BI
    ## 16  Jan.0-4016 Jan.0-4 Jan.0-403          8   Control
    ## 17  Jan.0-4017 Jan.0-4 Jan.0-404          8        BI
    ## 18  Jan.0-4018 Jan.0-4 Jan.0-404          8        BI
    ## 19  Jan.0-4019 Jan.0-4 Jan.0-404          8   Control
    ## 20  Jan.0-4020 Jan.0-4 Jan.0-404          8   Control
    ## 21  Jan.0-4021 Jan.0-4 Jan.0-404          8        BI
    ## 22  Jan.0-4022 Jan.0-4 Jan.0-404          8   Control
    ## 23  Jan.0-4023 Jan.0-4 Jan.0-404          8        BI
    ## 24  Jan.0-4024 Jan.0-4 Jan.0-404          8   Control
    ## 25  Jan.0-4025 Jan.0-4 Jan.0-405          8   Control
    ## 26  Jan.0-4026 Jan.0-4 Jan.0-405          8        BI
    ## 27  Jan.0-4027 Jan.0-4 Jan.0-405          8   Control
    ## 28  Jan.0-4028 Jan.0-4 Jan.0-405          8   Control
    ## 29  Jan.0-4029 Jan.0-4 Jan.0-405          8        BI
    ## 30  Jan.0-4030 Jan.0-4 Jan.0-405          8   Control
    ## 31  Jan.0-4031 Jan.0-4 Jan.0-405          8        BI
    ## 32  Jan.0-4032 Jan.0-4 Jan.0-405          8        BI
    ## 33  Jan.0-4033 Jan.0-4 Jan.0-406          8        BI
    ## 34  Jan.0-4034 Jan.0-4 Jan.0-406          8        BI
    ## 35  Jan.0-4035 Jan.0-4 Jan.0-406          8        BI
    ## 36  Jan.0-4036 Jan.0-4 Jan.0-406          8   Control
    ## 37  Jan.0-4037 Jan.0-4 Jan.0-406          8        BI
    ## 38  Jan.0-4038 Jan.0-4 Jan.0-406          8   Control
    ## 39  Jan.0-4039 Jan.0-4 Jan.0-406          8   Control
    ## 40  Jan.0-4040 Jan.0-4 Jan.0-406          8   Control
    ## 41  Jan.0-4041 Jan.0-4 Jan.0-407          2        BI
    ## 42  Jan.0-4042 Jan.0-4 Jan.0-407          2   Control
    ## 43  Jan.0-4043 Jan.0-4 Jan.0-408          6   Control
    ## 44  Jan.0-4044 Jan.0-4 Jan.0-408          6        BI
    ## 45  Jan.0-4045 Jan.0-4 Jan.0-408          6        BI
    ## 46  Jan.0-4046 Jan.0-4 Jan.0-408          6        BI
    ## 47  Jan.0-4047 Jan.0-4 Jan.0-408          6   Control
    ## 48  Jan.0-4048 Jan.0-4 Jan.0-408          6   Control
    ## 49  Jan.0-4049 Jan.0-4 Jan.0-409          8   Control
    ## 50  Jan.0-4050 Jan.0-4 Jan.0-409          8   Control
    ## 51  Jan.0-4051 Jan.0-4 Jan.0-409          8   Control
    ## 52  Jan.0-4052 Jan.0-4 Jan.0-409          8        BI
    ## 53  Jan.0-4053 Jan.0-4 Jan.0-409          8        BI
    ## 54  Jan.0-4054 Jan.0-4 Jan.0-409          8   Control
    ## 55  Jan.0-4055 Jan.0-4 Jan.0-409          8        BI
    ## 56  Jan.0-4056 Jan.0-4 Jan.0-409          8        BI
    ## 57  Jan.0-4057 Jan.0-4 Jan.0-410          2   Control
    ## 58  Jan.0-4058 Jan.0-4 Jan.0-410          2        BI
    ## 59  Jan.0-4059 Jan.0-4 Jan.0-411          2   Control
    ## 60  Jan.0-4060 Jan.0-4 Jan.0-411          2        BI
    ## 61  Jan.0-4061 Jan.0-4 Jan.0-412          4        BI
    ## 62  Jan.0-4062 Jan.0-4 Jan.0-412          4        BI
    ## 63  Jan.0-4063 Jan.0-4 Jan.0-412          4   Control
    ## 64  Jan.0-4064 Jan.0-4 Jan.0-412          4   Control
    ## 65  Jan.0-4065 Jan.0-4 Jan.0-413          4   Control
    ## 66  Jan.0-4066 Jan.0-4 Jan.0-413          4   Control
    ## 67  Jan.0-4067 Jan.0-4 Jan.0-413          4        BI
    ## 68  Jan.0-4068 Jan.0-4 Jan.0-413          4        BI
    ## 69  Jan.0-4069 Jan.0-4 Jan.0-414          2        BI
    ## 70  Jan.0-4070 Jan.0-4 Jan.0-414          2   Control
    ## 71  Jan.0-4071 Jan.0-4 Jan.0-415          8   Control
    ## 72  Jan.0-4072 Jan.0-4 Jan.0-415          8        BI
    ## 73  Jan.0-4073 Jan.0-4 Jan.0-415          8        BI
    ## 74  Jan.0-4074 Jan.0-4 Jan.0-415          8        BI
    ## 75  Jan.0-4075 Jan.0-4 Jan.0-415          8   Control
    ## 76  Jan.0-4076 Jan.0-4 Jan.0-415          8   Control
    ## 77  Jan.0-4077 Jan.0-4 Jan.0-415          8   Control
    ## 78  Jan.0-4078 Jan.0-4 Jan.0-415          8        BI
    ## 79  Jan.0-4079 Jan.0-4 Jan.0-416          4        BI
    ## 80  Jan.0-4080 Jan.0-4 Jan.0-416          4   Control
    ## 81  Jan.0-4081 Jan.0-4 Jan.0-416          4        BI
    ## 82  Jan.0-4082 Jan.0-4 Jan.0-416          4   Control
    ## 83  Jan.0-4083 Jan.0-4 Jan.0-417          8        BI
    ## 84  Jan.0-4084 Jan.0-4 Jan.0-417          8        BI
    ## 85  Jan.0-4085 Jan.0-4 Jan.0-417          8   Control
    ## 86  Jan.0-4086 Jan.0-4 Jan.0-417          8   Control
    ## 87  Jan.0-4087 Jan.0-4 Jan.0-417          8        BI
    ## 88  Jan.0-4088 Jan.0-4 Jan.0-417          8   Control
    ## 89  Jan.0-4089 Jan.0-4 Jan.0-417          8   Control
    ## 90  Jan.0-4090 Jan.0-4 Jan.0-417          8        BI
    ## 91  Jan.0-4091 Jan.0-4 Jan.0-418          2   Control
    ## 92  Jan.0-4092 Jan.0-4 Jan.0-418          2        BI
    ## 93  Jan.0-4093 Jan.0-4 Jan.0-419          8        BI
    ## 94  Jan.0-4094 Jan.0-4 Jan.0-419          8   Control
    ## 95  Jan.0-4095 Jan.0-4 Jan.0-419          8        BI
    ## 96  Jan.0-4096 Jan.0-4 Jan.0-419          8   Control
    ## 97  Jan.0-4097 Jan.0-4 Jan.0-419          8   Control
    ## 98  Jan.0-4098 Jan.0-4 Jan.0-419          8        BI
    ## 99  Jan.0-4099 Jan.0-4 Jan.0-419          8        BI
    ## 100 Jan.0-4100 Jan.0-4 Jan.0-419          8   Control
    ## 101 Jan.0-4101 Jan.0-4 Jan.0-420          4        BI
    ## 102 Jan.0-4102 Jan.0-4 Jan.0-420          4   Control
    ## 103 Jan.0-4103 Jan.0-4 Jan.0-420          4        BI
    ## 104 Jan.0-4104 Jan.0-4 Jan.0-420          4   Control
    ## 105 Jan.0-4105 Jan.0-4 Jan.0-421          2        BI
    ## 106 Jan.0-4106 Jan.0-4 Jan.0-421          2   Control
    ## 107 Jan.0-4107 Jan.0-4 Jan.0-422          6   Control
    ## 108 Jan.0-4108 Jan.0-4 Jan.0-422          6        BI
    ## 109 Jan.0-4109 Jan.0-4 Jan.0-422          6        BI
    ## 110 Jan.0-4110 Jan.0-4 Jan.0-422          6        BI
    ## 111 Jan.0-4111 Jan.0-4 Jan.0-422          6   Control
    ## 112 Jan.0-4112 Jan.0-4 Jan.0-422          6   Control
    ## 113 Jan.0-4113 Jan.0-4 Jan.0-423          8        BI
    ## 114 Jan.0-4114 Jan.0-4 Jan.0-423          8   Control
    ## 115 Jan.0-4115 Jan.0-4 Jan.0-423          8   Control
    ## 116 Jan.0-4116 Jan.0-4 Jan.0-423          8        BI
    ## 117 Jan.0-4117 Jan.0-4 Jan.0-423          8   Control
    ## 118 Jan.0-4118 Jan.0-4 Jan.0-423          8   Control
    ## 119 Jan.0-4119 Jan.0-4 Jan.0-423          8        BI
    ## 120 Jan.0-4120 Jan.0-4 Jan.0-423          8        BI
    ## 121 Jan.0-4121 Jan.0-4 Jan.0-424          2   Control
    ## 122 Jan.0-4122 Jan.0-4 Jan.0-424          2        BI
    ## 123 Jan.0-4123 Jan.0-4 Jan.0-425          6   Control
    ## 124 Jan.0-4124 Jan.0-4 Jan.0-425          6   Control
    ## 125 Jan.0-4125 Jan.0-4 Jan.0-425          6        BI
    ## 126 Jan.0-4126 Jan.0-4 Jan.0-425          6   Control
    ## 127 Jan.0-4127 Jan.0-4 Jan.0-425          6        BI
    ## 128 Jan.0-4128 Jan.0-4 Jan.0-425          6        BI
    ## 129 Jan.0-4129 Jan.0-4 Jan.0-426          8        BI
    ## 130 Jan.0-4130 Jan.0-4 Jan.0-426          8        BI
    ## 131 Jan.0-4131 Jan.0-4 Jan.0-426          8   Control
    ## 132 Jan.0-4132 Jan.0-4 Jan.0-426          8   Control
    ## 133 Jan.0-4133 Jan.0-4 Jan.0-426          8        BI
    ## 134 Jan.0-4134 Jan.0-4 Jan.0-426          8        BI
    ## 135 Jan.0-4135 Jan.0-4 Jan.0-426          8   Control
    ## 136 Jan.0-4136 Jan.0-4 Jan.0-426          8   Control
    ## 137 Jan.0-4137 Jan.0-4 Jan.0-427          2        BI
    ## 138 Jan.0-4138 Jan.0-4 Jan.0-427          2   Control
    ## 139 Jan.0-4139 Jan.0-4 Jan.0-428          6   Control
    ## 140 Jan.0-4140 Jan.0-4 Jan.0-428          6        BI
    ## 141 Jan.0-4141 Jan.0-4 Jan.0-428          6        BI
    ## 142 Jan.0-4142 Jan.0-4 Jan.0-428          6        BI
    ## 143 Jan.0-4143 Jan.0-4 Jan.0-428          6   Control
    ## 144 Jan.0-4144 Jan.0-4 Jan.0-428          6   Control
    ## 145 Jan.0-4145 Jan.0-4 Jan.0-429          6        BI
    ## 146 Jan.0-4146 Jan.0-4 Jan.0-429          6        BI
    ## 147 Jan.0-4147 Jan.0-4 Jan.0-429          6   Control
    ## 148 Jan.0-4148 Jan.0-4 Jan.0-429          6        BI
    ## 149 Jan.0-4149 Jan.0-4 Jan.0-429          6   Control
    ## 150 Jan.0-4150 Jan.0-4 Jan.0-429          6   Control
    ## 151 Jan.0-4151 Jan.0-4 Jan.0-430          8        BI
    ## 152 Jan.0-4152 Jan.0-4 Jan.0-430          8   Control
    ## 153 Jan.0-4153 Jan.0-4 Jan.0-430          8        BI
    ## 154 Jan.0-4154 Jan.0-4 Jan.0-430          8   Control
    ## 155 Jan.0-4155 Jan.0-4 Jan.0-430          8        BI
    ## 156 Jan.0-4156 Jan.0-4 Jan.0-430          8        BI
    ## 157 Jan.0-4157 Jan.0-4 Jan.0-430          8   Control
    ## 158 Jan.0-4158 Jan.0-4 Jan.0-430          8   Control
    ## 159 Jan.0-4159 Jan.0-4 Jan.0-431          2        BI
    ## 160 Jan.0-4160 Jan.0-4 Jan.0-431          2   Control
    ## 161 Jan.0-4161 Jan.0-4 Jan.0-432          2   Control
    ## 162 Jan.0-4162 Jan.0-4 Jan.0-432          2        BI
    ## 163 Jan.0-4163 Jan.0-4 Jan.0-433          2   Control
    ## 164 Jan.0-4164 Jan.0-4 Jan.0-433          2        BI
    ## 165 Jan.0-4165 Jan.0-4 Jan.0-434          2        BI
    ## 166 Jan.0-4166 Jan.0-4 Jan.0-434          2   Control
    ## 167 Jan.0-4167 Jan.0-4 Jan.0-435          6        BI
    ## 168 Jan.0-4168 Jan.0-4 Jan.0-435          6   Control
    ## 169 Jan.0-4169 Jan.0-4 Jan.0-435          6        BI
    ## 170 Jan.0-4170 Jan.0-4 Jan.0-435          6   Control
    ## 171 Jan.0-4171 Jan.0-4 Jan.0-435          6        BI
    ## 172 Jan.0-4172 Jan.0-4 Jan.0-435          6   Control
    ## 173 Jan.0-4173 Jan.0-4 Jan.0-436          2        BI
    ## 174 Jan.0-4174 Jan.0-4 Jan.0-436          2   Control
    ## 175 Jan.0-4175 Jan.0-4 Jan.0-437          6   Control
    ## 176 Jan.0-4176 Jan.0-4 Jan.0-437          6        BI
    ## 177 Jan.0-4177 Jan.0-4 Jan.0-437          6        BI
    ## 178 Jan.0-4178 Jan.0-4 Jan.0-437          6        BI
    ## 179 Jan.0-4179 Jan.0-4 Jan.0-437          6   Control
    ## 180 Jan.0-4180 Jan.0-4 Jan.0-437          6   Control
    ## 181 Jan.0-4181 Jan.0-4 Jan.0-438          8        BI
    ## 182 Jan.0-4182 Jan.0-4 Jan.0-438          8        BI
    ## 183 Jan.0-4183 Jan.0-4 Jan.0-438          8   Control
    ## 184 Jan.0-4184 Jan.0-4 Jan.0-438          8        BI
    ## 185 Jan.0-4185 Jan.0-4 Jan.0-438          8   Control
    ## 186 Jan.0-4186 Jan.0-4 Jan.0-438          8   Control
    ## 187 Jan.0-4187 Jan.0-4 Jan.0-438          8        BI
    ## 188 Jan.0-4188 Jan.0-4 Jan.0-438          8   Control
    ## 189 Jan.0-4189 Jan.0-4 Jan.0-439          2   Control
    ## 190 Jan.0-4190 Jan.0-4 Jan.0-439          2        BI
    ## 191 Jan.0-4191 Jan.0-4 Jan.0-440          4        BI
    ## 192 Jan.0-4192 Jan.0-4 Jan.0-440          4   Control
    ## 193 Jan.0-4193 Jan.0-4 Jan.0-440          4   Control
    ## 194 Jan.0-4194 Jan.0-4 Jan.0-440          4        BI
    ## 195 Jan.0-4195 Jan.0-4 Jan.0-441          6        BI
    ## 196 Jan.0-4196 Jan.0-4 Jan.0-441          6        BI
    ## 197 Jan.0-4197 Jan.0-4 Jan.0-441          6   Control
    ## 198 Jan.0-4198 Jan.0-4 Jan.0-441          6   Control
    ## 199 Jan.0-4199 Jan.0-4 Jan.0-441          6   Control
    ## 200 Jan.0-4200 Jan.0-4 Jan.0-441          6        BI
    ## 201 Jan.0-4201 Jan.0-4 Jan.0-442          2        BI
    ## 202 Jan.0-4202 Jan.0-4 Jan.0-442          2   Control
    ## 203 Jan.0-4203 Jan.0-4 Jan.0-443          2        BI
    ## 204 Jan.0-4204 Jan.0-4 Jan.0-443          2   Control
    ## 205 Jan.0-4205 Jan.0-4 Jan.0-444          8   Control
    ## 206 Jan.0-4206 Jan.0-4 Jan.0-444          8   Control
    ## 207 Jan.0-4207 Jan.0-4 Jan.0-444          8        BI
    ## 208 Jan.0-4208 Jan.0-4 Jan.0-444          8        BI
    ## 209 Jan.0-4209 Jan.0-4 Jan.0-444          8        BI
    ## 210 Jan.0-4210 Jan.0-4 Jan.0-444          8   Control
    ## 211 Jan.0-4211 Jan.0-4 Jan.0-444          8   Control
    ## 212 Jan.0-4212 Jan.0-4 Jan.0-444          8        BI
    ## 213 Jan.0-4213 Jan.0-4 Jan.0-445          6        BI
    ## 214 Jan.0-4214 Jan.0-4 Jan.0-445          6        BI
    ## 215 Jan.0-4215 Jan.0-4 Jan.0-445          6   Control
    ## 216 Jan.0-4216 Jan.0-4 Jan.0-445          6   Control
    ## 217 Jan.0-4217 Jan.0-4 Jan.0-445          6        BI
    ## 218 Jan.0-4218 Jan.0-4 Jan.0-445          6   Control
    ## 219 Jan.0-4219 Jan.0-4 Jan.0-446          2   Control
    ## 220 Jan.0-4220 Jan.0-4 Jan.0-446          2        BI
    ## 221 Jan.0-4221 Jan.0-4 Jan.0-447          2   Control
    ## 222 Jan.0-4222 Jan.0-4 Jan.0-447          2        BI
    ## 223 Jan.0-4223 Jan.0-4 Jan.0-448          8        BI
    ## 224 Jan.0-4224 Jan.0-4 Jan.0-448          8   Control
    ## 225 Jan.0-4225 Jan.0-4 Jan.0-448          8   Control
    ## 226 Jan.0-4226 Jan.0-4 Jan.0-448          8   Control
    ## 227 Jan.0-4227 Jan.0-4 Jan.0-448          8        BI
    ## 228 Jan.0-4228 Jan.0-4 Jan.0-448          8        BI
    ## 229 Jan.0-4229 Jan.0-4 Jan.0-448          8   Control
    ## 230 Jan.0-4230 Jan.0-4 Jan.0-448          8        BI
    ## 231 Jan.0-4231 Jan.0-4 Jan.0-449          6        BI
    ## 232 Jan.0-4232 Jan.0-4 Jan.0-449          6   Control
    ## 233 Jan.0-4233 Jan.0-4 Jan.0-449          6   Control
    ## 234 Jan.0-4234 Jan.0-4 Jan.0-449          6        BI
    ## 235 Jan.0-4235 Jan.0-4 Jan.0-449          6   Control
    ## 236 Jan.0-4236 Jan.0-4 Jan.0-449          6        BI
    ## 237 Jan.0-4237 Jan.0-4 Jan.0-450          2   Control
    ## 238 Jan.0-4238 Jan.0-4 Jan.0-450          2        BI
    ## 239 Jan.0-4239 Jan.0-4 Jan.0-451          8        BI
    ## 240 Jan.0-4240 Jan.0-4 Jan.0-451          8   Control
    ## 241 Jan.0-4241 Jan.0-4 Jan.0-451          8        BI
    ## 242 Jan.0-4242 Jan.0-4 Jan.0-451          8   Control
    ## 243 Jan.0-4243 Jan.0-4 Jan.0-451          8        BI
    ## 244 Jan.0-4244 Jan.0-4 Jan.0-451          8   Control
    ## 245 Jan.0-4245 Jan.0-4 Jan.0-451          8        BI
    ## 246 Jan.0-4246 Jan.0-4 Jan.0-451          8   Control
    ## 247 Jan.0-4247 Jan.0-4 Jan.0-452          2        BI
    ## 248 Jan.0-4248 Jan.0-4 Jan.0-452          2   Control
    ## 249 Jan.0-4249 Jan.0-4 Jan.0-453          6   Control
    ## 250 Jan.0-4250 Jan.0-4 Jan.0-453          6   Control
    ## 251 Jan.0-4251 Jan.0-4 Jan.0-453          6        BI
    ## 252 Jan.0-4252 Jan.0-4 Jan.0-453          6        BI
    ## 253 Jan.0-4253 Jan.0-4 Jan.0-453          6   Control
    ## 254 Jan.0-4254 Jan.0-4 Jan.0-453          6        BI
    ## 255 Jan.0-4255 Jan.0-4 Jan.0-454          4        BI
    ## 256 Jan.0-4256 Jan.0-4 Jan.0-454          4        BI
    ## 257 Jan.0-4257 Jan.0-4 Jan.0-454          4   Control
    ## 258 Jan.0-4258 Jan.0-4 Jan.0-454          4   Control
    ## 259 Jan.0-4259 Jan.0-4 Jan.0-455          2   Control
    ## 260 Jan.0-4260 Jan.0-4 Jan.0-455          2        BI
    ## 261 Jan.0-4261 Jan.0-4 Jan.0-456          6   Control
    ## 262 Jan.0-4262 Jan.0-4 Jan.0-456          6   Control
    ## 263 Jan.0-4263 Jan.0-4 Jan.0-456          6        BI
    ## 264 Jan.0-4264 Jan.0-4 Jan.0-456          6   Control
    ## 265 Jan.0-4265 Jan.0-4 Jan.0-456          6        BI
    ## 266 Jan.0-4266 Jan.0-4 Jan.0-456          6        BI
    ## 267 Jan.0-4267 Jan.0-4 Jan.0-457          4   Control
    ## 268 Jan.0-4268 Jan.0-4 Jan.0-457          4   Control
    ## 269 Jan.0-4269 Jan.0-4 Jan.0-457          4        BI
    ## 270 Jan.0-4270 Jan.0-4 Jan.0-457          4        BI
    ## 271 Jan.0-4271 Jan.0-4 Jan.0-458          2        BI
    ## 272 Jan.0-4272 Jan.0-4 Jan.0-458          2   Control
    ## 273 Jan.0-4273 Jan.0-4 Jan.0-459          2        BI
    ## 274 Jan.0-4274 Jan.0-4 Jan.0-459          2   Control
    ## 275 Jan.0-4275 Jan.0-4 Jan.0-460          2        BI
    ## 276 Jan.0-4276 Jan.0-4 Jan.0-460          2   Control
    ## 277 Jan.0-4277 Jan.0-4 Jan.0-461          8        BI
    ## 278 Jan.0-4278 Jan.0-4 Jan.0-461          8        BI
    ## 279 Jan.0-4279 Jan.0-4 Jan.0-461          8   Control
    ## 280 Jan.0-4280 Jan.0-4 Jan.0-461          8   Control
    ## 281 Jan.0-4281 Jan.0-4 Jan.0-461          8   Control
    ## 282 Jan.0-4282 Jan.0-4 Jan.0-461          8        BI
    ## 283 Jan.0-4283 Jan.0-4 Jan.0-461          8        BI
    ## 284 Jan.0-4284 Jan.0-4 Jan.0-461          8   Control
    ## 285 Jan.0-4285 Jan.0-4 Jan.0-462          8        BI
    ## 286 Jan.0-4286 Jan.0-4 Jan.0-462          8   Control
    ## 287 Jan.0-4287 Jan.0-4 Jan.0-462          8        BI
    ## 288 Jan.0-4288 Jan.0-4 Jan.0-462          8   Control
    ## 289 Jan.0-4289 Jan.0-4 Jan.0-462          8   Control
    ## 290 Jan.0-4290 Jan.0-4 Jan.0-462          8        BI
    ## 291 Jan.0-4291 Jan.0-4 Jan.0-462          8        BI
    ## 292 Jan.0-4292 Jan.0-4 Jan.0-462          8   Control
    ## 293 Jan.0-4293 Jan.0-4 Jan.0-463          8        BI
    ## 294 Jan.0-4294 Jan.0-4 Jan.0-463          8   Control
    ## 295 Jan.0-4295 Jan.0-4 Jan.0-463          8   Control
    ## 296 Jan.0-4296 Jan.0-4 Jan.0-463          8   Control
    ## 297 Jan.0-4297 Jan.0-4 Jan.0-463          8   Control
    ## 298 Jan.0-4298 Jan.0-4 Jan.0-463          8        BI
    ## 299 Jan.0-4299 Jan.0-4 Jan.0-463          8        BI
    ## 300 Jan.0-4300 Jan.0-4 Jan.0-463          8        BI
    ## 301 Jan.0-4301 Jan.0-4 Jan.0-464          8        BI
    ## 302 Jan.0-4302 Jan.0-4 Jan.0-464          8        BI
    ## 303 Jan.0-4303 Jan.0-4 Jan.0-464          8        BI
    ## 304 Jan.0-4304 Jan.0-4 Jan.0-464          8   Control
    ## 305 Jan.0-4305 Jan.0-4 Jan.0-464          8        BI
    ## 306 Jan.0-4306 Jan.0-4 Jan.0-464          8   Control
    ## 307 Jan.0-4307 Jan.0-4 Jan.0-464          8   Control
    ## 308 Jan.0-4308 Jan.0-4 Jan.0-464          8   Control
    ## 309 Jan.0-4309 Jan.0-4 Jan.0-465          4   Control
    ## 310 Jan.0-4310 Jan.0-4 Jan.0-465          4        BI
    ## 311 Jan.0-4311 Jan.0-4 Jan.0-465          4        BI
    ## 312 Jan.0-4312 Jan.0-4 Jan.0-465          4   Control
    ## 313 Jan.0-4313 Jan.0-4 Jan.0-466          2   Control
    ## 314 Jan.0-4314 Jan.0-4 Jan.0-466          2        BI
    ## 315 Jan.0-4315 Jan.0-4 Jan.0-467          2   Control
    ## 316 Jan.0-4316 Jan.0-4 Jan.0-467          2        BI
    ## 317 Jan.0-4317 Jan.0-4 Jan.0-468          2        BI
    ## 318 Jan.0-4318 Jan.0-4 Jan.0-468          2   Control
    ## 319 Jan.0-4319 Jan.0-4 Jan.0-469          6        BI
    ## 320 Jan.0-4320 Jan.0-4 Jan.0-469          6        BI
    ## 321 Jan.0-4321 Jan.0-4 Jan.0-469          6   Control
    ## 322 Jan.0-4322 Jan.0-4 Jan.0-469          6   Control
    ## 323 Jan.0-4323 Jan.0-4 Jan.0-469          6        BI
    ## 324 Jan.0-4324 Jan.0-4 Jan.0-469          6   Control
    ## 325 Jan.0-4325 Jan.0-4 Jan.0-470          6   Control
    ## 326 Jan.0-4326 Jan.0-4 Jan.0-470          6        BI
    ## 327 Jan.0-4327 Jan.0-4 Jan.0-470          6        BI
    ## 328 Jan.0-4328 Jan.0-4 Jan.0-470          6   Control
    ## 329 Jan.0-4329 Jan.0-4 Jan.0-470          6   Control
    ## 330 Jan.0-4330 Jan.0-4 Jan.0-470          6        BI
    ## 331 Jan.0-4331 Jan.0-4 Jan.0-471          2   Control
    ## 332 Jan.0-4332 Jan.0-4 Jan.0-471          2        BI
    ## 333 Jan.0-4333 Jan.0-4 Jan.0-472          2   Control
    ## 334 Jan.0-4334 Jan.0-4 Jan.0-472          2        BI
    ## 335 Jan.0-4335 Jan.0-4 Jan.0-473          8        BI
    ## 336 Jan.0-4336 Jan.0-4 Jan.0-473          8   Control
    ## 337 Jan.0-4337 Jan.0-4 Jan.0-473          8   Control
    ## 338 Jan.0-4338 Jan.0-4 Jan.0-473          8        BI
